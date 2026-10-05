;;;; math/special-functions.lisp --- the normal distribution, expm1 and log1p

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
               (loop for a across #(3.1611237438705656d0 113.864154151050156d0
                                    377.485237685302021d0)
                     for b across #(23.6012909523441209d0 244.024637934444173d0
                                    1282.61652607737228d0)
                     do (setf xnum (* (+ xnum (the double-float a)) ysq)
                              xden (* (+ xden (the double-float b)) ysq)))
               (setf result (/ (* x (+ xnum 3209.37758913846947d0))
                               (+ xden 2844.23683343917062d0)))
               (unless (zerop jint) (setf result (- 1d0 result)))
               (when (= jint 2) (setf result (* (exp ysq) result)))
               (return-from cody-calerf result)))
            ((<= y 4d0)
             (let ((xnum (* 2.15311535474403846d-8 y)) (xden y))
               (declare (double-float xnum xden))
               (loop for c across #(0.564188496988670089d0 8.88314979438837594d0
                                    66.1191906371416295d0 298.635138197400131d0
                                    881.95222124176909d0 1712.04761263407058d0
                                    2051.07837782607147d0)
                     for d across #(15.7449261107098347d0 117.693950891312499d0
                                    537.181101862009858d0 1621.38957456669019d0
                                    3290.79923573345963d0 4362.61909014324716d0
                                    3439.36767414372164d0)
                     do (setf xnum (* (+ xnum (the double-float c)) y)
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
                 (loop for p across #(0.305326634961232344d0 0.360344899949804439d0
                                      0.125781726111229246d0 0.0160837851487422766d0)
                       for q across #(2.56852019228982242d0 1.87295284992346047d0
                                      0.527905102951428412d0 0.0605183413124413191d0)
                       do (setf xnum (* (+ xnum (the double-float p)) ysq)
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
