;;;; math/special-functions.lisp --- the normal distribution, expm1 and log1p,
;;;; erfcx, modified Bessel K and the noncentral chi-square distribution

(in-package #:fincl)

(defconstant +dbl-epsilon+ #.(scale-float 1d0 -52) "C's DBL_EPSILON, 2^-52.")
(defconstant +sqrt-dbl-epsilon+ #.(sqrt (scale-float 1d0 -52)))
(defconstant +one-over-sqrt-two+ 0.7071067811865475244008443621048490392848359376887d0)

(declaim (inline norm-pdf))

(declaim (ftype (function (double-float) (values double-float &optional)) norm-pdf))
(defun norm-pdf (x)
  "Standard normal density.

  (norm-pdf 0d0) => 0.3989422804014327d0"
  (declare (double-float x))
  (* 0.3989422804014327d0 (exp (* -0.5d0 x x))))

(defmacro horner (x &rest coefficients)
  "C0 + X (C1 + X (C2 + ...)), expanded inline: (horner r c0 c1 c2)."
  (a:with-gensyms (v)
    `(let ((,v ,x))
       (declare (double-float ,v))
       ,(reduce (lambda (c acc) `(+ ,c (* ,v ,acc)))
                (butlast coefficients)
                :from-end t :initial-value (a:lastcar coefficients)))))

(defun as241 (p)
  "Wichura's algorithm AS241 (PPND16): the normal quantile of P in (0, 1/2],
to about 1e-16 relative. The tails go through sqrt(-log P), so P down to the
least positive double works."
  (declare (double-float p))
  (let ((q (- p 0.5d0)))
    (if (<= (abs q) 0.425d0)
        (let ((r (- 0.180625d0 (* q q))))
          (/ (* q (horner r 3.387132872796366608d0 133.14166789178437745d0
                          1971.5909503065514427d0 13731.693765509461125d0
                          45921.953931549871457d0 67265.770927008700853d0
                          33430.575583588128105d0 2509.0809287301226727d0))
             (horner r 1d0 42.313330701600911252d0 687.1870074920579083d0
                     5394.1960214247511077d0 21213.794301586595867d0
                     39307.89580009271061d0 28729.085735721942674d0
                     5226.495278852545925d0)))
        (let ((r (sqrt (- (log p)))))
          (- (if (<= r 5d0)
                 (let ((r (- r 1.6d0)))
                   (/ (horner r 1.42343711074968357734d0 4.6303378461565452959d0
                              5.7694972214606914055d0 3.64784832476320460504d0
                              1.27045825245236838258d0 0.24178072517745061177d0
                              0.0227238449892691845833d0 7.7454501427834140764d-4)
                      (horner r 1d0 2.05319162663775882187d0 1.6763848301838038494d0
                              0.68976733498510000455d0 0.14810397642748007459d0
                              0.0151986665636164571966d0 5.475938084995344946d-4
                              1.05075007164441684324d-9)))
                 (let ((r (- r 5d0)))
                   (/ (horner r 6.6579046435011037772d0 5.4637849111641143699d0
                              1.7848265399172913358d0 0.29656057182850489123d0
                              0.026532189526576123093d0 0.0012426609473880784386d0
                              2.71155556874348757815d-5 2.01033439929228813265d-7)
                      (horner r 1d0 0.59983220655588793769d0 0.13692988092273580531d0
                              0.0148753612908506148525d0 7.868691311456132591d-4
                              1.8463183175100546818d-5 1.4215117583164458887d-7
                              2.04426310338993978564d-15)))))))))

(declaim (ftype (function (double-float) (values double-float &optional)) norm-quantile))
(defun norm-quantile (u)
  "Inverse standard normal CDF for U in (0, 1): Wichura's AS241, then one
Halley step on norm-cdf. Computed on the smaller tail, min(U, 1 - U), and
reflected, so (norm-quantile u) = (- (norm-quantile (- 1 u))) exactly
whenever 1 - U is exact (U >= 1/2).

  (norm-quantile 0.975d0) => 1.959963984540054d0
  (norm-quantile 1d-300) => -37.0471...d0"
  (declare (double-float u))
  (if (> u 0.5d0)
      (- (norm-quantile (- 1d0 u)))
      (let* ((x (as241 u))
             ;; Halley: e = (Phi(x) - u) / phi(x), x <- x - e / (1 + x e / 2).
             (e (/ (- (norm-cdf x) u) (norm-pdf x))))
        (declare (double-float x e))
        (- x (/ e (+ 1d0 (* 0.5d0 x e)))))))

;;; --------------------------------------------------------------------
;;; exp(x) - 1 and log(1 + x) without cancellation near 0
;;; --------------------------------------------------------------------

(declaim (ftype (function (double-float) (values double-float &optional)) expm1))
(defun expm1 (x)
  "e^X - 1, accurate to a few ulps also for |X| near 0, where (- (exp x) 1)
loses every digit. Kahan's method: with u = e^x rounded, (u - 1) x / log u
cancels the rounding error of u.

  (expm1 1d-10) => 1.00000000005d-10"
  (declare (double-float x))
  (if (> (abs x) 0.5d0)
      (- (exp x) 1d0)
      (let ((u (exp x)))
        (declare (double-float u))
        (if (= u 1d0)
            x
            (/ (* (- u 1d0) x) (log u))))))

(declaim (ftype (function (double-float)
                    (values (or double-float (complex double-float)) &optional))
          log1p))
(defun log1p (x)
  "log(1 + X) for X > -1, accurate to a few ulps also for |X| near 0, where
(log (+ 1 x)) loses every digit. Kahan's method: with u = 1 + x rounded,
log(u) x / (u - 1) cancels the rounding error of u.

  (log1p 1d-10) => 9.9999999995d-11"
  (declare (double-float x))
  (if (> (abs x) 0.5d0)
      (log (+ 1d0 x))
      (let ((u (+ 1d0 x)))
        (declare (double-float u))
        (if (= u 1d0)
            x
            (/ (* (log u) x) (- u 1d0))))))

;;; --------------------------------------------------------------------
;;; Complex square root and logarithm
;;; --------------------------------------------------------------------
;;;
;;; SBCL's SQRT and LOG on a (complex double-float) go through CSSQS, which
;;; saves and restores the floating-point modes on every call and boxes the
;;; argument and the result; in a characteristic function that is more than
;;; the arithmetic. These are inline, on the real and imaginary parts, and
;;; defer to SQRT and LOG outside |z| in [1e-150, 1e150], where those scale
;;; to avoid overflow.

(declaim (inline complex-sqrt complex-log complex-log1p))

(declaim (ftype (function ((complex double-float))
                    (values (complex double-float) &optional))
          complex-sqrt))
(defun complex-sqrt (z)
  "Principal square root of the (complex double-float) Z, with non-negative
real part; the sign of a zero imaginary part is kept, so the branch cut is as
SQRT's.

  (complex-sqrt #c(-4d0 0d0)) => #C(0.0d0 2.0d0)"
  (declare (type (complex double-float) z))
  (let* ((x (realpart z)) (y (imagpart z))
         (r (libm-hypot x y)))
    (declare (double-float x y r))
    (cond ((zerop r) (complex 0d0 y))
          ((not (< 1d-150 r 1d150)) (sqrt z))
          (t (let ((s (sqrt (the (double-float 0d0) (* 0.5d0 (+ (abs x) r))))))
               (declare (double-float s))
               (if (>= x 0d0)
                   (complex s (/ y (* 2d0 s)))
                   (complex (/ (abs y) (* 2d0 s)) (float-sign y s))))))))

(declaim (ftype (function ((complex double-float))
                    (values (complex double-float) &optional))
          complex-log))
(defun complex-log (z)
  "Principal logarithm of the (complex double-float) Z, imaginary part in
(-pi, pi]. Near |Z| = 1 the real part is log1p((a - 1)(a + 1) + b^2)/2 with
a >= b the larger and smaller of |Re Z| and |Im Z|, which avoids the
cancellation in log |Z|.

  (complex-log #c(0d0 1d0)) => #C(0.0d0 1.5707963267948966d0)"
  (declare (type (complex double-float) z))
  (let* ((x (realpart z)) (y (imagpart z))
         (r (libm-hypot x y)))
    (declare (double-float x y r))
    (if (not (< 1d-150 r 1d150))
        (log z)
        (let* ((ax (abs x)) (ay (abs y))
               (a (max ax ay)) (b (min ax ay)))
          (declare (double-float ax ay a b))
          (complex (if (< 0.5d0 r 2d0)
                       (* 0.5d0 (libm-log1p (+ (* (- a 1d0) (+ a 1d0)) (* b b))))
                       (libm-log r))
                   (atan y x))))))

(declaim (ftype (function ((complex double-float))
                    (values (complex double-float) &optional))
          complex-log1p))
(defun complex-log1p (z)
  "log(1 + Z) for the (complex double-float) Z, accurate also for |Z| near 0
(Kahan's method, as LOG1P)."
  (declare (type (complex double-float) z))
  (let ((u (+ 1d0 z)))
    (if (= u 1d0)
        z
        (/ (* (complex-log u) z) (- u 1d0)))))

;;; --------------------------------------------------------------------
;;; Cody's erf, erfc and erfcx
;;; --------------------------------------------------------------------
;;;
;;; W. J. Cody, "Rational Chebyshev approximations for the error function",
;;; Math. Comp. 23 (1969) 631-638; the netlib specfun CALERF routine, as
;;; adapted in Peter Jäckel's LetsBeRational distribution. erfcx(x) =
;;; exp(x^2) erfc(x) is evaluated without any exponential for x > 4, which is
;;; what makes deep out-of-the-money Black prices accurate (see
;;; lets-be-rational.lisp).

(declaim (ftype (function (double-float (integer 0 2))
                    (values double-float &optional))
          cody-calerf))
(defun cody-calerf (x jint)
  "erf(X) for JINT 0, erfc(X) for JINT 1, exp(X^2) erfc(X) for JINT 2."
  (declare (double-float x) (type (integer 0 2) jint)
           (optimize speed))
  (let ((y (abs x)) (result 0d0))
    (declare (double-float y result))
    (flet ((scaled-exp (z)
             ;; exp(-z^2) as exp(-zr^2) exp(-del), zr = z rounded to 1/16:
             ;; Cody's guard against the rounding error of z^2.
             (declare (double-float z))
             (let* ((ysq (/ (ftruncate (* z 16d0)) 16d0))
                    (del (* (- z ysq) (+ z ysq))))
               (* (exp (- (* ysq ysq))) (exp (- del))))))
      (cond ((<= y 0.46875d0)
             (let* ((ysq (if (> y 1.11d-16) (* y y) 0d0))
                    (xnum (* 0.185777706184603153d0 ysq))
                    (xden ysq))
               (declare (double-float ysq xnum xden))
               (iter (for a in-vector #(3.1611237438705656d0 113.864154151050156d0
                                        377.485237685302021d0))
                     (for b in-vector #(23.6012909523441209d0 244.024637934444173d0
                                        1282.61652607737228d0))
                     (setf xnum (* (+ xnum (the double-float a)) ysq)
                           xden (* (+ xden (the double-float b)) ysq)))
               (setf result (/ (* x (+ xnum 3209.37758913846947d0))
                               (+ xden 2844.23683343917062d0)))
               (unless (zerop jint) (setf result (- 1d0 result)))
               (when (= jint 2) (setf result (* (exp ysq) result)))
               (return-from cody-calerf result)))
            ((<= y 4d0)
             (let ((xnum (* 2.15311535474403846d-8 y)) (xden y))
               (declare (double-float xnum xden))
               (iter (for c in-vector #(0.564188496988670089d0 8.88314979438837594d0
                                        66.1191906371416295d0 298.635138197400131d0
                                        881.95222124176909d0 1712.04761263407058d0
                                        2051.07837782607147d0))
                     (for d in-vector #(15.7449261107098347d0 117.693950891312499d0
                                        537.181101862009858d0 1621.38957456669019d0
                                        3290.79923573345963d0 4362.61909014324716d0
                                        3439.36767414372164d0))
                     (setf xnum (* (+ xnum (the double-float c)) y)
                           xden (* (+ xden (the double-float d)) y)))
               (setf result (/ (+ xnum 1230.33935479799725d0) (+ xden 1230.33935480374942d0)))
               (unless (= jint 2) (setf result (* (scaled-exp y) result)))))
            (t
             (block large
               (when (>= y 26.543d0)
                 (when (or (/= jint 2) (>= y 2.53d307)) (return-from large))
                 (when (>= y 6.71d7)
                   (setf result (/ 0.56418958354775628695d0 y))
                   (return-from large)))
               (let* ((ysq (/ (* y y)))
                      (xnum (* 0.0163153871373020978d0 ysq))
                      (xden ysq))
                 (declare (double-float ysq xnum xden))
                 (iter (for p in-vector #(0.305326634961232344d0 0.360344899949804439d0
                                          0.125781726111229246d0 0.0160837851487422766d0))
                       (for q in-vector #(2.56852019228982242d0 1.87295284992346047d0
                                          0.527905102951428412d0 0.0605183413124413191d0))
                       (setf xnum (* (+ xnum (the double-float p)) ysq)
                             xden (* (+ xden (the double-float q)) ysq)))
                 (setf result (/ (* ysq (+ xnum 6.58749161529837803d-4))
                                 (+ xden 0.00233520497626869185d0))
                       result (/ (- 0.56418958354775628695d0 result) y))
                 (unless (= jint 2) (setf result (* (scaled-exp y) result)))))))
      ;; Negative arguments.
      (ecase jint
        (0 (setf result (+ (- 0.5d0 result) 0.5d0))
         (when (< x 0d0) (setf result (- result))))
        (1 (when (< x 0d0) (setf result (- 2d0 result))))
        (2 (when (< x 0d0)
             (setf result (if (< x -26.628d0)
                              most-positive-double-float
                              (let* ((ysq (/ (ftruncate (* x 16d0)) 16d0))
                                     (del (* (- x ysq) (+ x ysq))))
                                (- (* 2d0 (exp (* ysq ysq)) (exp del)) result)))))))
      result)))

(declaim (ftype (function (real) (values double-float &optional)) erfcx))
(defun erfcx (x)
  "The scaled complementary error function exp(X^2) erfc(X), by Cody's
rational approximations, without overflow or cancellation for large X.

  (erfcx 10d0) => 0.05614099274382259d0"
  (cody-calerf (float x 1d0) 2))

(declaim (ftype (function (double-float) (values double-float &optional)) norm-cdf))
(defun norm-cdf (x)
  "Standard normal CDF: Cody's erfc, and below -10 the asymptotic series of
Abramowitz and Stegun 26.2.12, as in Jäckel's LetsBeRational, so the left
tail keeps its relative precision.

  (norm-cdf 1.96d0) => 0.9750021048517795d0"
  (declare (double-float x))
  (if (<= x -10d0)
      (let ((sum 1d0))
        (declare (double-float sum))
        (when (>= x (- (/ +sqrt-dbl-epsilon+)))
          (let ((zsqr (* x x)) (i 1d0) (g 1d0) (a most-positive-double-float) (lasta 0d0))
            (declare (double-float zsqr i g a lasta))
            (iter (setf lasta a)
                  (let* ((u (/ (- (* 4 i) 3) zsqr))
                         (y (* u (/ (- (* 4 i) 1) zsqr))))
                    (declare (double-float u y))
                    (setf a (* g (- u y)))
                    (decf sum a)
                    (setf g (* g y))
                    (incf i)
                    (setf a (abs a)))
                  (while (and (> lasta a) (>= a (abs (* sum +dbl-epsilon+))))))))
        (/ (* (- (norm-pdf x)) sum) x))
      (* 0.5d0 (cody-calerf (* (- x) +one-over-sqrt-two+) 1))))

;;; --------------------------------------------------------------------
;;; Modified Bessel function of the second kind, K_nu(x)
;;; --------------------------------------------------------------------
;;;
;;; N. M. Temme, "On the numerical evaluation of the modified Bessel function
;;; of the third kind", J. Comput. Phys. 19 (1975) 324-337, as arranged in
;;; Numerical Recipes' BESSIK: write nu = n + mu with |mu| <= 1/2, find K_mu
;;; and K_mu+1 by Temme's series for x < 2 or Steed's continued fraction
;;; (CF2) for x >= 2, then recur upwards in the order, which is stable for K.

(defparameter *reciprocal-gamma-coefficients*
  (coerce '(1d0 0.5772156649015329d0 -0.6558780715202538d0 -0.0420026350340952d0
            0.1665386113822915d0 -0.0421977345555443d0 -0.0096219715278770d0
            0.0072189432466630d0 -0.0011651675918591d0 -0.0002152416741149d0
            0.0001280502823882d0 -0.0000201348547807d0 -0.0000012504934821d0
            0.0000011330272320d0 -0.0000002056338417d0 0.0000000061160950d0
            0.0000000050020075d0 -0.0000000011812746d0 0.0000000001043427d0
            0.0000000000077823d0 -0.0000000000036968d0 0.0000000000005100d0
            -0.0000000000000206d0 -0.0000000000000054d0 0.0000000000000014d0
            0.0000000000000001d0)
          '(simple-array double-float (*)))
  "c_1 .. c_26 of 1/Gamma(z) = sum c_k z^k (Abramowitz and Stegun 6.1.34), so
1/Gamma(1 + z) = sum c_k z^(k-1).")

(defun temme-gammas (mu)
  "Temme's (values gamma1 gamma2 1/Gamma(1+MU) 1/Gamma(1-MU)) for |MU| <= 1/2:
gamma1 = (1/Gamma(1-mu) - 1/Gamma(1+mu)) / (2 mu), gamma2 = their mean, from
the even and odd parts of the series of 1/Gamma(1 + z), with no cancellation
at small MU."
  (declare (double-float mu))
  (let ((c *reciprocal-gamma-coefficients*) (even 0d0) (odd 0d0) (mu2 (* mu mu)))
    (declare (double-float even odd mu2))
    ;; Coefficient c_(k+1) multiplies z^k: even k into EVEN, odd k into ODD.
    (iter (for k from (1- (length c)) downto 0)
          (if (evenp k)
              (setf even (+ (aref c k) (* mu2 even)))
              (setf odd (+ (aref c k) (* mu2 odd)))))
    (values (- odd) even (+ even (* mu odd)) (- even (* mu odd)))))

(defun bessel-k-mu (mu x)
  "(values K_MU(X) K_MU+1(X)) for |MU| <= 1/2 and X > 0."
  (declare (double-float mu x))
  (let ((eps 1d-17))
    (if (< x 2d0)
        ;; Temme's series.
        (let* ((x2 (* 0.5d0 x))
               (pimu (* pi mu))
               (fact (if (< (abs pimu) eps) 1d0 (/ pimu (sin pimu))))
               (d (- (log x2)))
               (e (* mu d))
               (fact2 (if (< (abs e) eps) 1d0 (/ (sinh e) e))))
          (multiple-value-bind (gam1 gam2 gampl gammi) (temme-gammas mu)
            (let* ((ff (* fact (+ (* gam1 (cosh e)) (* gam2 fact2 d))))
                   (sum ff)
                   (ee (exp e))
                   (p (/ (* 0.5d0 ee) gampl))
                   (q (/ 0.5d0 (* ee gammi)))
                   (c 1d0)
                   (dd (* x2 x2))
                   (sum1 p))
              (declare (double-float ff sum ee p q c dd sum1))
              (iter (for i from 1 to 10000)
                    (setf ff (/ (+ (* i ff) p q) (- (* i i) (* mu mu)))
                          c (* c (/ dd i))
                          p (/ p (- i mu))
                          q (/ q (+ i mu)))
                    (let ((del (* c ff)))
                      (incf sum del)
                      (incf sum1 (* c (- p (* i ff))))
                      (when (< (abs del) (* (abs sum) eps)) (finish))))
              (values sum (* sum1 (/ 2d0 x))))))
        ;; Steed's continued fraction CF2, with Temme's normalization.
        (let* ((b (* 2d0 (+ 1d0 x)))
               (d (/ b))
               (h d) (delh d)
               (q1 0d0) (q2 1d0)
               (a1 (- 0.25d0 (* mu mu)))
               (q a1) (c a1) (a (- a1))
               (s (+ 1d0 (* q delh))))
          (declare (double-float b d h delh q1 q2 a1 q c a s))
          (iter (for i from 2 to 100000)
                (decf a (* 2 (1- i)))
                (setf c (/ (* (- a) c) i))
                (let ((qnew (/ (- q1 (* b q2)) a)))
                  (setf q1 q2 q2 qnew)
                  (incf q (* c qnew)))
                (incf b 2d0)
                (setf d (/ (+ b (* a d)))
                      delh (* (- (* b d) 1d0) delh))
                (incf h delh)
                (let ((dels (* q delh)))
                  (incf s dels)
                  (when (< (abs (/ dels s)) eps) (finish))))
          (let ((kmu (/ (* (sqrt (/ pi (* 2d0 x))) (exp (- x))) s)))
            (values kmu (/ (* kmu (- (+ mu x 0.5d0) (* a1 h))) x)))))))

(declaim (ftype (function (t t) (values double-float &optional)) bessel-k))
(defun bessel-k (nu x)
  "The modified Bessel function of the second kind K_NU(X), for real NU and
X > 0 (K_-nu = K_nu). Relative accuracy about 1e-15.

  (bessel-k 1 0.5d0) => 1.6564411200033008d0"
  (check-input nu real "a real order")
  (check-input x (real (0)) "a positive real")
  (let* ((nu (abs (float nu 1d0)))
         (x (float x 1d0))
         (n (floor (+ nu 0.5d0)))
         (mu (- nu n)))
    (multiple-value-bind (k k1) (bessel-k-mu mu x)
      (declare (double-float k k1))
      ;; K_(m+1) = 2 m / x K_m + K_(m-1), upwards from m = mu + 1.
      (iter (for i from 1 to n)
            (psetf k k1
                   k1 (+ (* (/ (* 2d0 (+ mu i)) x) k1) k)))
      k)))

;;; --------------------------------------------------------------------
;;; Noncentral chi-square distribution
;;; --------------------------------------------------------------------
;;;
;;; D. Benton and K. Krishnamoorthy, "Computing discrete mixtures of
;;; continuous distributions: noncentral chisquare, noncentral t and the
;;; distribution of the square of the sample multiple correlation
;;; coefficient", Comput. Stat. Data Anal. 43 (2003) 249-267. The CDF is a
;;; Poisson(lambda/2) mixture of central chi-square CDFs, P(f/2 + k, x/2) in
;;; the regularized incomplete gamma P. The sum starts at the Poisson mode,
;;; where the weights are largest, and runs both ways with the recurrences
;;; P(a + 1, y) = P(a, y) - y^a e^-y / Gamma(a + 1) for the gamma terms and
;;; ratios for the weights, so only one incomplete gamma is evaluated. The
;;; complement is summed the same way from Q = 1 - P, so upper tails keep
;;; their relative precision.

(declaim (ftype (function (real) (values double-float &optional)) log-gamma))
(defun log-gamma (x)
  "log Gamma(X) for X > 0, as a double-float."
  (float (sf:log-gamma (float x 1d0)) 1d0))

(defconstant +gamma-series-max-shape+ 1d5
  "Above this shape the series and continued fraction of REGULARIZED-GAMMA
need thousands of terms near X = A; SF:INCOMPLETE-GAMMA's asymptotic
expansion takes over.")

(declaim (inline flush-subnormal))
(defun flush-subnormal (y)
  "Y, or 0 if it is below the least normalized double: a subnormal keeps
too few bits to be a relative-precision result, and callers (the noncentral
chi-square recursion) start afresh from 0."
  (declare (double-float y))
  (if (< (abs y) least-positive-normalized-double-float) 0d0 y))

(defun gamma-series (a x)
  "P(A, X) for X < A + 1: x^a e^-x / Gamma(a + 1) times sum_n x^n /
((a + 1) ... (a + n)), the prefix by POISSON-DENSITY. NIL if the series has
not converged after 20,000 terms."
  (declare (double-float a x) (optimize speed))
  (let ((sum 1d0) (term 1d0))
    (declare (double-float sum term))
    (iter (declare (declare-variables))
          (for (the fixnum n) from 1 to 20000)
          (setf term (* term (/ x (+ a n))))
          (incf sum term)
          (when (< term (* sum 1d-17))
            (return-from gamma-series (flush-subnormal (* (poisson-density a x) sum)))))
    nil))

(defun gamma-continued-fraction (a x)
  "Q(A, X) for X >= A + 1: x^a e^-x / Gamma(a) times Legendre's continued
fraction 1/(x + 1 - a - 1 (1 - a)/(x + 3 - a - ...)), by the modified Lentz
method. NIL if it has not converged after 20,000 terms."
  (declare (double-float a x) (optimize speed))
  (let* ((tiny 1d-300)
         (b (+ x 1d0 (- a)))
         (c (/ tiny))
         (d (/ b))
         (h d))
    (declare (double-float tiny b c d h))
    (iter (declare (declare-variables))
          (for (the fixnum i) from 1 to 20000)
          (let ((an (* (- i) (- i a))))
            (declare (double-float an))
            (incf b 2d0)
            (setf d (+ (* an d) b))
            (when (< (abs d) tiny) (setf d tiny))
            (setf c (+ b (/ an c)))
            (when (< (abs c) tiny) (setf c tiny))
            (setf d (/ d))
            (let ((delta (* d c)))
              (declare (double-float delta))
              (setf h (* h delta))
              (when (< (abs (- delta 1d0)) 1d-16)
                (return-from gamma-continued-fraction
                  (flush-subnormal (* a (poisson-density a x) h)))))))
    nil))

(declaim (ftype (function (real real)
                    (values double-float double-float &optional))
          regularized-gamma))
(defun regularized-gamma (a x)
  "(values P(A, X) Q(A, X)), the regularized incomplete gamma functions, as
double-floats: the series for P below X = A + 1 and the continued fraction
for Q above, each with the other as its complement (which is not small in
that region, so it keeps its relative precision); SF:INCOMPLETE-GAMMA beyond
+GAMMA-SERIES-MAX-SHAPE+ or if they do not converge.

  (regularized-gamma 2d0 1d0) => 0.2642411176571153d0, 0.7357588823428847d0"
  (let ((a (float a 1d0)) (x (float x 1d0)))
    (flet ((fallback ()
             (multiple-value-bind (p q) (sf:incomplete-gamma a x)
               (values (float p 1d0) (float q 1d0)))))
      (cond ((<= x 0d0) (values 0d0 1d0))
            ((> a +gamma-series-max-shape+) (fallback))
            ((< x (+ a 1d0))
             (a:if-let ((p (gamma-series a x))) (values p (- 1d0 p)) (fallback)))
            (t
             (a:if-let ((q (gamma-continued-fraction a x))) (values (- 1d0 q) q) (fallback)))))))

(defun stirling-error (x)
  "log Gamma(X + 1) - ((X + 1/2) log X - X + log(2 pi)/2), the error of
Stirling's formula, for X > 0: by its asymptotic series above 15, where
the difference itself would cancel, directly below (C. Loader, \"Fast and
accurate computation of binomial probabilities\", 2000)."
  (declare (double-float x))
  (let ((s0 (/ 1d0 12)) (s1 (/ 1d0 360)) (s2 (/ 1d0 1260)) (s3 (/ 1d0 1680))
        (s4 (/ 1d0 1188)))
    (if (<= x 15d0)
        (- (log-gamma (+ x 1d0)) (* (+ x 0.5d0) (log x)) (- x) (* 0.5d0 (log (* 2 pi))))
        (let ((x2 (/ 1d0 (* x x))))
          (/ (cond ((> x 500d0) (- s0 (* s1 x2)))
                   ((> x 80d0) (- s0 (* x2 (- s1 (* s2 x2)))))
                   ((> x 35d0) (- s0 (* x2 (- s1 (* x2 (- s2 (* s3 x2)))))))
                   (t (- s0 (* x2 (- s1 (* x2 (- s2 (* x2 (- s3 (* s4 x2))))))))))
             x)))))

(defun bd0 (x m)
  "X log(X / M) + M - X, the deviance term of the Poisson density, without
the cancellation of its three large terms when X is near M (Loader 2000)."
  (declare (double-float x m))
  (if (< (abs (- x m)) (* 0.1d0 (+ x m)))
      (let* ((v (/ (- x m) (+ x m)))
             (s (* (- x m) v))
             (ej (* 2d0 x v))
             (v2 (* v v)))
        (declare (double-float v s ej v2))
        (iter (for j from 1 to 1000)
              (setf ej (* ej v2))
              (let ((next (+ s (/ ej (+ (* 2 j) 1)))))
                (when (= next s) (return))
                (setf s next)))
        s)
      (+ (* x (log (/ x m))) m (- x))))

(defun poisson-density (x m)
  "M^X e^-M / Gamma(X + 1) for real X >= 0 and M > 0: the Poisson
probability at integer X, and the gamma-prefix term of the incomplete gamma
recurrences otherwise. Saddle-point form exp(-stirlerr(X) - bd0(X, M)) /
sqrt(2 pi X), accurate to a few ulps also when X and M are 1e10."
  (declare (double-float x m))
  (if (zerop x)
      (exp (- m))
      (/ (exp (- (- (stirling-error x)) (bd0 x m))) (sqrt (* 2 pi x)))))

;;; The P - term and Q - term recurrences lose absolute precision, about eps
;;; times the P (or Q) of the last evaluation, which matters only against the
;;; sum it enters: the terms where it is large have small Poisson weights.
(defconstant +gamma-anchor-error+ 8.8d-16
  "Assumed relative error of a REGULARIZED-GAMMA evaluation, 8 ulp.")
(defconstant +gamma-sum-tolerance+ 1d-14
  "Fraction of the running sum that the recurrences' accumulated error may reach
before the incomplete gamma is evaluated afresh.")

(declaim (ftype (function (t t t)
                    (values double-float double-float &optional))
          noncentral-chi-square-cdf))
(defun noncentral-chi-square-cdf (x df lambda)
  "(values F 1-F): the noncentral chi-square distribution function with DF
degrees of freedom (DF > 0) and noncentrality LAMBDA >= 0 at X, and its
complement, each with full relative precision.

  (noncentral-chi-square-cdf 3d0 2d0 1d0) => 0.6081..., 0.3918..."
  (check-input x real "a real")
  (check-input df (real (0)) "positive degrees of freedom")
  (check-input lambda (real 0) "a non-negative noncentrality")
  (let ((x (float x 1d0)) (df (float df 1d0)) (lambda (float lambda 1d0)))
    (when (<= x 0d0)
      (return-from noncentral-chi-square-cdf (values 0d0 1d0)))
    (let* ((y (* 0.5d0 x))
           (half (* 0.5d0 lambda))
           (k (floor half))
           (a (+ (* 0.5d0 df) k))
           (eps 1d-17)
           ;; Poisson weight at the mode, and y^a e^-y / Gamma(a + 1).
           (w0 (if (zerop half) 1d0 (poisson-density (float k 1d0) half)))
           (t0 (poisson-density a y)))
      (declare (double-float y half a w0 t0) (fixnum k))
      (multiple-value-bind (p0 q0) (regularized-gamma a y)
        (declare (double-float p0 q0))
        (let ((p-sum (* w0 p0)) (q-sum (* w0 q0)))
          (declare (double-float p-sum q-sum))
          ;; Forward from the mode: weights fall, P falls, Q rises to 1.
          (let ((w w0) (p p0) (q q0) (term t0)
                (err (* +gamma-anchor-error+ p0)) (spent 0d0))
            (declare (double-float w p q term err spent))
            ;; The Poisson weights spread over about sqrt(lambda/2) terms.
            (iter (for i from 1 to (+ 10000 (ceiling (* 60 (sqrt half)))))
                  (setf w (/ (* w half) (+ k i)))
                  ;; P - term loses absolute precision (about eps times the
                  ;; last anchor's P); evaluate P afresh only when the error
                  ;; spent so far would pass the tolerance of the sum.
                  (let ((err2 (+ err (* 2d0 double-float-epsilon term))))
                    (declare (double-float err2))
                    (if (> (+ spent (* w err2)) (* +gamma-sum-tolerance+ p-sum))
                        (progn (multiple-value-setq (p q) (regularized-gamma (+ a i) y))
                               (setf err (* +gamma-anchor-error+ p)))
                        (setf p (- p term) q (+ q term) err err2))
                    (incf spent (* w err)))
                  (setf term (/ (* term y) (+ a i)))
                  (incf p-sum (* w p))
                  (incf q-sum (* w q))
                  ;; The weights beyond i fall faster than a geometric
                  ;; series of ratio R, which bounds both remainders.
                  (let* ((r (/ half (+ k i 1)))
                         (tail (/ (* w r) (- 1d0 r))))
                    (when (and (<= (* tail p) (* eps p-sum))
                               (<= tail (* eps q-sum)))
                      (finish)))))
          ;; Backward from the mode to k = 0: P rises, Q falls.
          (let ((w w0) (p p0) (q q0)
                (err (* +gamma-anchor-error+ q0)) (spent 0d0)
                (term (/ (* t0 a) y)))  ; y^(a-1) e^-y / Gamma(a)
            (declare (double-float w p q term err spent))
            (iter (for j from k above 0)
                  (for aj = (- a (- k j)))  ; shape at index j
                  (setf w (/ (* w j) half))
                  ;; Likewise Q - term; and afresh when P or the term
                  ;; underflowed, far in the lower tail.
                  (let ((err2 (+ err (* 2d0 double-float-epsilon term))))
                    (declare (double-float err2))
                    (if (or (> (+ spent (* w err2)) (* +gamma-sum-tolerance+ q-sum))
                            (zerop p) (zerop term))
                        (progn
                          (multiple-value-setq (p q) (regularized-gamma (- aj 1d0) y))
                          (setf err (* +gamma-anchor-error+ q)
                                term (if (> aj 2d0) (poisson-density (- aj 2d0) y) 0d0)))
                        (setf p (+ p term) q (- q term) err err2
                              term (/ (* term (- aj 1d0)) y)))
                    (incf spent (* w err)))
                  (incf p-sum (* w p))
                  (incf q-sum (* w q))
                  (let* ((r (/ (- j 1) half))
                         (tail (/ (* w r) (- 1d0 r))))
                    (when (and (<= tail (* eps p-sum)) (<= (* tail q) (* eps q-sum)))
                      (finish)))))
          (values (min 1d0 p-sum) (min 1d0 q-sum)))))))

;;; --------------------------------------------------------------------
;;; Bivariate normal
;;; --------------------------------------------------------------------

;;; Gauss-Legendre nodes and weights on [-1, 1], one half each (the rule is
;;; symmetric), for 6, 12 and 20 points.
(a:define-constant +bivariate-legendre+
    '(((0.9324695142031522d0 . 0.1713244923791705d0) (0.6612093864662647d0 . 0.3607615730481384d0)
       (0.2386191860831970d0 . 0.4679139345726904d0))
      ((0.9815606342467191d0 . 0.04717533638651177d0) (0.9041172563704750d0 . 0.1069393259953183d0)
       (0.7699026741943050d0 . 0.1600783285433464d0) (0.5873179542866171d0 . 0.2031674267230659d0)
       (0.3678314989981802d0 . 0.2334925365383547d0) (0.1252334085114692d0 . 0.2491470458134029d0))
      ((0.9931285991850949d0 . 0.01761400713915212d0) (0.9639719272779138d0 . 0.04060142980038694d0)
       (0.9122344282513259d0 . 0.06267204833410906d0) (0.8391169718222188d0 . 0.08327674157670475d0)
       (0.7463319064601508d0 . 0.1019301198172404d0) (0.6360536807265150d0 . 0.1181945319615184d0)
       (0.5108670019508271d0 . 0.1316886384491766d0) (0.3737060887154196d0 . 0.1420961093183821d0)
       (0.2277858511416451d0 . 0.1491729864726037d0) (0.07652652113349733d0 . 0.1527533871307259d0)))
  :test #'equal)

(defun bivariate-normal-upper (h k r)
  "P(X > H, Y > K) for standard normals X, Y with correlation R: Genz's
algorithm (Drezner and Wesolowsky's Gauss-Legendre integral over asin r
for |R| < 0.925, an asymptotic expansion about |R| = 1 beyond), accurate to
about 1e-15. A. Genz, \"Numerical computation of rectangular bivariate and
trivariate normal and t probabilities\", Statistics and Computing 14 (2004)."
  (declare (double-float h k r))
  (when (zerop r)
    (return-from bivariate-normal-upper (* (norm-cdf (- h)) (norm-cdf (- k)))))
  (let* ((two-pi (* 2d0 pi))
         (rule (nth (cond ((< (abs r) 0.3d0) 0) ((< (abs r) 0.75d0) 1) (t 2)) +bivariate-legendre+))
         ;; The full rule on [0, 2]: nodes 1 -/+ x.
         (nodes (iter (for (x . w) in rule)
                      (collect `(,(- 1d0 x) . ,w))
                      (collect `(,(+ 1d0 x) . ,w))))
         (hk (* h k))
         (bvn 0d0))
    (declare (double-float hk bvn))
    (if (< (abs r) 0.925d0)
        (let ((hs (* 0.5d0 (+ (* h h) (* k k))))
              (asr (* 0.5d0 (asin r))))
          (iter (for (x . w) in nodes)
                (let ((sn (sin (* asr x))))
                  (incf bvn (* w (exp (/ (- (* sn hk) hs) (- 1d0 (* sn sn))))))))
          (setf bvn (+ (/ (* bvn asr) two-pi) (* (norm-cdf (- h)) (norm-cdf (- k))))))
        (progn
          (when (minusp r) (setf k (- k) hk (- hk)))
          (when (< (abs r) 1d0)
            (let* ((as (- 1d0 (* r r)))
                   (a (sqrt as))
                   (bs (expt (- h k) 2))
                   (asr (* -0.5d0 (+ (/ bs as) hk)))
                   (c (/ (- 4d0 hk) 8d0))
                   (d (/ (- 12d0 hk) 80d0)))
              (when (> asr -100d0)
                (setf bvn (* a (exp asr) (+ (- 1d0 (/ (* c (- bs as) (- 1d0 (* d bs))) 3d0))
                                            (* c d as as)))))
              (when (> hk -100d0)
                (let ((b (sqrt bs)))
                  (decf bvn (* (exp (* -0.5d0 hk)) (sqrt two-pi) (norm-cdf (- (/ b a))) b
                               (- 1d0 (/ (* c bs (- 1d0 (* d bs))) 3d0))))))
              (let ((a (* 0.5d0 a))
                    (sum 0d0))
                (declare (double-float sum))
                (iter (for (x . w) in nodes)
                      (let* ((xs (expt (* a x) 2))
                             (asr (* -0.5d0 (+ (/ bs xs) hk))))
                        (when (> asr -100d0)
                          (let* ((sp (+ 1d0 (* c xs (+ 1d0 (* 5d0 d xs)))))
                                 (rs (sqrt (- 1d0 xs)))
                                 (ep (/ (exp (/ (* -0.5d0 hk xs) (expt (+ 1d0 rs) 2))) rs)))
                            (incf sum (* w (exp asr) (- sp ep)))))))
                (setf bvn (/ (- (* a sum) bvn) two-pi)))))
          (cond ((plusp r) (incf bvn (norm-cdf (- (max h k)))))
                ((>= h k) (setf bvn (- bvn)))
                (t (setf bvn (- (if (minusp h)
                                    (- (norm-cdf k) (norm-cdf h))
                                    (- (norm-cdf (- h)) (norm-cdf (- k))))
                                bvn))))))
    (max 0d0 (min 1d0 bvn))))

(declaim (ftype (function (real real real) (values double-float &optional)) bivariate-norm-cdf))
(defun bivariate-norm-cdf (a b rho)
  "P(X <= A, Y <= B) for standard normals X, Y with correlation RHO.

  (bivariate-norm-cdf 0d0 0d0 0.5d0) => 0.3333333333333333d0"
  (bivariate-normal-upper (- (float a 1d0)) (- (float b 1d0)) (float rho 1d0)))
