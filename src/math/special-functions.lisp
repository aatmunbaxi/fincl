;;;; math/special-functions.lisp --- the normal distribution, expm1 and log1p,
;;;; erfcx, modified Bessel K and the noncentral chi-square distribution

(in-package #:fincl)

(defconstant +sqrt2+ (sqrt 2d0))

(declaim (inline norm-cdf norm-pdf))

(defun norm-cdf (x)
  "Standard normal CDF.

  (norm-cdf 1.96d0) => 0.9750021048517795d0"
  (declare (double-float x))
  (* 0.5d0 (sf:erfc (/ (- x) +sqrt2+))))

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
;;; Cody's erf, erfc and erfcx
;;; --------------------------------------------------------------------
;;;
;;; W. J. Cody, "Rational Chebyshev approximations for the error function",
;;; Math. Comp. 23 (1969) 631-638; the netlib specfun CALERF routine, as
;;; adapted in Peter Jäckel's LetsBeRational distribution. erfcx(x) =
;;; exp(x^2) erfc(x) is evaluated without any exponential for x > 4, which is
;;; what makes deep out-of-the-money Black prices accurate (see
;;; lets-be-rational.lisp).

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

(defun erfcx (x)
  "The scaled complementary error function exp(X^2) erfc(X), by Cody's
rational approximations, without overflow or cancellation for large X.

  (erfcx 10d0) => 0.05614099274382259d0"
  (cody-calerf (float x 1d0) 2))

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

(defun log-gamma (x)
  "log Gamma(X) for X > 0, as a double-float."
  (float (sf:log-gamma (float x 1d0)) 1d0))

(defun regularized-gamma (a x)
  "(values P(A, X) Q(A, X)), the regularized incomplete gamma functions, as
double-floats (SF:INCOMPLETE-GAMMA returns integers in some branches)."
  (multiple-value-bind (p q) (sf:incomplete-gamma (float a 1d0) (float x 1d0))
    (values (float p 1d0) (float q 1d0))))

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
          (let ((w w0) (p p0) (q q0) (term t0))
            (declare (double-float w p q term))
            ;; The Poisson weights spread over about sqrt(lambda/2) terms.
            (iter (for i from 1 to (+ 10000 (ceiling (* 60 (sqrt half)))))
                  (setf w (/ (* w half) (+ k i)))
                  ;; P - term cancels when the term is a sizeable part of
                  ;; P (Y well below the shape): evaluate P afresh there.
                  (if (> term (* 0.05d0 p))
                      (multiple-value-setq (p q) (regularized-gamma (+ a i) y))
                      (setf p (- p term) q (+ q term)))
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
                (term (/ (* t0 a) y)))  ; y^(a-1) e^-y / Gamma(a)
            (declare (double-float w p q term))
            (iter (for j from k above 0)
                  (for aj = (- a (- k j)))  ; shape at index j
                  (setf w (/ (* w j) half))
                  ;; Likewise Q - term, when Y is well above the shape;
                  ;; and when P or the term underflowed, far in the lower
                  ;; tail.
                  (if (or (> term (* 0.05d0 q)) (zerop p) (zerop term))
                      (progn
                        (multiple-value-setq (p q) (regularized-gamma (- aj 1d0) y))
                        (setf term (if (> aj 2d0) (poisson-density (- aj 2d0) y) 0d0)))
                      (setf p (+ p term) q (- q term)
                            term (/ (* term (- aj 1d0)) y)))
                  (incf p-sum (* w p))
                  (incf q-sum (* w q))
                  (let* ((r (/ (- j 1) half))
                         (tail (/ (* w r) (- 1d0 r))))
                    (when (and (<= tail (* eps p-sum)) (<= (* tail q) (* eps q-sum)))
                      (finish)))))
          (values (min 1d0 p-sum) (min 1d0 q-sum)))))))
