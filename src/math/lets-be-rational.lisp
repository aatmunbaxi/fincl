;;;; math/lets-be-rational.lisp --- Black implied volatility to machine precision
;;;;
;;;; A port of Peter Jäckel's reference implementation of "Let's Be Rational"
;;;; (Wilmott, January 2015, pp. 40-53), from LetsBeRational.cpp and
;;;; rationalcubic.cpp of www.jaeckel.org/LetsBeRational.7z (the 2013-2014
;;;; sources, as mirrored in github.com/vollib/lets_be_rational). Its notice:
;;;;
;;;; ======================================================================================
;;;; Copyright © 2013-2014 Peter Jäckel.
;;;;
;;;; Permission to use, copy, modify, and distribute this software is freely granted,
;;;; provided that this notice is preserved.
;;;;
;;;; WARRANTY DISCLAIMER
;;;; The Software is provided "as is" without warranty of any kind, either express or implied,
;;;; including without limitation any implied warranties of condition, uninterrupted use,
;;;; merchantability, fitness for a particular purpose, or non-infringement.
;;;; ======================================================================================
;;;;
;;;; Everything is in normalised form: with x = ln(F/K) and s = sigma sqrt(T),
;;;; the Black price divided by sqrt(F K) is
;;;;
;;;;   b(x, s) = theta [exp(x/2) Phi(theta (x/s + s/2)) - exp(-x/2) Phi(theta (x/s - s/2))].
;;;;
;;;; The inversion maps in-the-money to out-of-the-money (subtracting the
;;;; intrinsic value) and puts to calls, starts from a rational-cubic guess in
;;;; one of four branches, and takes at most two Householder(3) steps on an
;;;; objective function chosen per branch so the result is accurate to the
;;;; attainable precision. The reference's DENORMALIZATION_CUTOFF is 0, so its
;;;; denormal checks never fire and are omitted; so are its compile-time
;;;; options (iteration counts as output, other Householder orders).
;;;;
;;;; The expressions follow the C++ operation for operation, so results
;;;; match it to the last bits. The two expansions, nested 35 and 14 deep in
;;;; the C++, are written as -> chains from the innermost level out: + and *
;;;; are exactly commutative in IEEE arithmetic, so each threaded step is the
;;;; C++'s operation on the same operands. tests/implied-volatility.lisp
;;;; checks the port against vectors from the compiled reference.

(in-package #:fincl)

(defconstant +dbl-epsilon+ #.(scale-float 1d0 -52) "C's DBL_EPSILON, 2^-52.")
(defconstant +sqrt-dbl-epsilon+ #.(sqrt (scale-float 1d0 -52)))
(defconstant +fourth-root-dbl-epsilon+ #.(sqrt (sqrt (scale-float 1d0 -52))))
(defconstant +sixteenth-root-dbl-epsilon+ #.(sqrt (sqrt (sqrt (sqrt (scale-float 1d0 -52))))))
(defconstant +sqrt-dbl-min+ #.(sqrt least-positive-normalized-double-float))
(defconstant +sqrt-dbl-max+ #.(sqrt most-positive-double-float))

(defconstant +one-over-sqrt-two+ 0.7071067811865475244008443621048490392848359376887d0)
(defconstant +one-over-sqrt-two-pi+ 0.3989422804014326779399460599343818684758586311649d0)
(defconstant +sqrt-two-pi+ 2.506628274631000502415765284811045253006986740610d0)
(defconstant +two-pi+ 6.283185307179586476925286766559005768394338798750d0)
(defconstant +sqrt-pi-over-two+ 1.253314137315500251207882642405522626503493370305d0)
(defconstant +sqrt-three+ 1.732050807568877293527446341505872366942805253810d0)
(defconstant +sqrt-one-over-three+ 0.577350269189625764509148780501957455647601751270d0)
(defconstant +two-pi-over-sqrt-twenty-seven+ 1.209199576156145233729385505094770488189377498728d0)
(defconstant +pi-over-six+ 0.523598775598298873077107230546583814032861566563d0)

(defconstant +lbr-maximum-iterations+ 2)
(defconstant +asymptotic-expansion-accuracy-threshold+ -10d0 "eta")
(defconstant +small-t-expansion-of-normalized-black-threshold+
  (* 2 +sixteenth-root-dbl-epsilon+) "tau")

(declaim (inline lbr-norm-pdf square))
(defun lbr-norm-pdf (x)
  (declare (double-float x))
  (* +one-over-sqrt-two-pi+ (exp (* -0.5d0 x x))))
(defun square (x) (declare (double-float x)) (* x x))

(defun lbr-norm-cdf (z)
  "Phi(Z) as in the reference: Abramowitz and Stegun 26.2.12 below -10,
Cody's erfc above."
  (declare (double-float z))
  (if (<= z -10d0)
      (let ((sum 1d0))
        (declare (double-float sum))
        (when (>= z (- (/ +sqrt-dbl-epsilon+)))
          (let ((zsqr (* z z)) (i 1d0) (g 1d0) (a most-positive-double-float) (lasta 0d0))
            (declare (double-float zsqr i g a lasta))
            (loop do (setf lasta a)
                     (let* ((x (/ (- (* 4 i) 3) zsqr))
                            (y (* x (/ (- (* 4 i) 1) zsqr))))
                       (declare (double-float x y))
                       (setf a (* g (- x y)))
                       (decf sum a)
                       (setf g (* g y))
                       (incf i)
                       (setf a (abs a)))
                  while (and (> lasta a) (>= a (abs (* sum +dbl-epsilon+)))))))
        (/ (* (- (lbr-norm-pdf z)) sum) z))
      (* 0.5d0 (cody-calerf (* (- z) +one-over-sqrt-two+) 1))))

;;; --------------------------------------------------------------------
;;; Rational cubic interpolation (Delbourgo and Gregory, 1985)
;;; --------------------------------------------------------------------

(defconstant +minimum-rational-cubic-control-parameter-value+
  (- (- 1 (sqrt (scale-float 1d0 -52)))))
(defconstant +maximum-rational-cubic-control-parameter-value+
  (/ 2 (* (scale-float 1d0 -52) (scale-float 1d0 -52))))

(declaim (inline rational-cubic-zero-p))
(defun rational-cubic-zero-p (x)
  (declare (double-float x))
  (< (abs x) least-positive-normalized-double-float))

(defun rational-cubic-interpolation (x x-l x-r y-l y-r d-l d-r r)
  (declare (double-float x x-l x-r y-l y-r d-l d-r r))
  (let ((h (- x-r x-l)))
    (if (<= (abs h) 0d0)
        (* 0.5d0 (+ y-l y-r))
        (let ((tt (/ (- x x-l) h)))
          (if (not (>= r +maximum-rational-cubic-control-parameter-value+))
              (let* ((omt (- 1 tt)) (t2 (* tt tt)) (omt2 (* omt omt)))
                ;; Formula (2.4) divided by formula (2.5).
                (/ (+ (* y-r t2 tt) (* (- (* r y-r) (* h d-r)) t2 omt)
                      (* (+ (* r y-l) (* h d-l)) tt omt2) (* y-l omt2 omt))
                   (+ 1 (* (- r 3) tt omt))))
              ;; Linear interpolation without over- or underflow.
              (+ (* y-r tt) (* y-l (- 1 tt))))))))

(defun rational-cubic-control-parameter-to-fit-second-derivative-at-left-side
    (x-l x-r y-l y-r d-l d-r second-derivative-l)
  (declare (double-float x-l x-r y-l y-r d-l d-r second-derivative-l))
  (let* ((h (- x-r x-l))
         (numerator (+ (* 0.5d0 h second-derivative-l) (- d-r d-l))))
    (if (rational-cubic-zero-p numerator)
        0d0
        (let ((denominator (- (/ (- y-r y-l) h) d-l)))
          (cond ((not (rational-cubic-zero-p denominator)) (/ numerator denominator))
                ((> numerator 0) +maximum-rational-cubic-control-parameter-value+)
                (t +minimum-rational-cubic-control-parameter-value+))))))

(defun rational-cubic-control-parameter-to-fit-second-derivative-at-right-side
    (x-l x-r y-l y-r d-l d-r second-derivative-r)
  (declare (double-float x-l x-r y-l y-r d-l d-r second-derivative-r))
  (let* ((h (- x-r x-l))
         (numerator (+ (* 0.5d0 h second-derivative-r) (- d-r d-l))))
    (if (rational-cubic-zero-p numerator)
        0d0
        (let ((denominator (- d-r (/ (- y-r y-l) h))))
          (cond ((not (rational-cubic-zero-p denominator)) (/ numerator denominator))
                ((> numerator 0) +maximum-rational-cubic-control-parameter-value+)
                (t +minimum-rational-cubic-control-parameter-value+))))))

(defun minimum-rational-cubic-control-parameter (d-l d-r s prefer-shape-preservation)
  (declare (double-float d-l d-r s))
  (let ((monotonic (and (>= (* d-l s) 0) (>= (* d-r s) 0)))
        (convex (and (<= d-l s) (<= s d-r)))
        (concave (and (>= d-l s) (>= s d-r))))
    (if (and (not monotonic) (not convex) (not concave))
        +minimum-rational-cubic-control-parameter-value+
        (let ((d-r-m-d-l (- d-r d-l)) (d-r-m-s (- d-r s)) (s-m-d-l (- s d-l))
              (r1 (- most-positive-double-float)) (r2 (- most-positive-double-float)))
          (declare (double-float d-r-m-d-l d-r-m-s s-m-d-l r1 r2))
          ;; If monotonicity on this interval is possible, set r1 to satisfy
          ;; the monotonicity condition (3.8).
          (when monotonic
            (cond ((not (rational-cubic-zero-p s)) (setf r1 (/ (+ d-r d-l) s)))
                  (prefer-shape-preservation
                   (setf r1 +maximum-rational-cubic-control-parameter-value+))))
          (cond ((or convex concave)
                 (cond ((not (or (rational-cubic-zero-p s-m-d-l) (rational-cubic-zero-p d-r-m-s)))
                        ;; (3.18)
                        (setf r2 (max (abs (/ d-r-m-d-l d-r-m-s)) (abs (/ d-r-m-d-l s-m-d-l)))))
                       (prefer-shape-preservation
                        (setf r2 +maximum-rational-cubic-control-parameter-value+))))
                ((and monotonic prefer-shape-preservation)
                 (setf r2 +maximum-rational-cubic-control-parameter-value+)))
          (max +minimum-rational-cubic-control-parameter-value+ (max r1 r2))))))

(defun convex-rational-cubic-control-parameter-to-fit-second-derivative-at-left-side
    (x-l x-r y-l y-r d-l d-r second-derivative-l prefer-shape-preservation)
  (max (rational-cubic-control-parameter-to-fit-second-derivative-at-left-side
        x-l x-r y-l y-r d-l d-r second-derivative-l)
       (minimum-rational-cubic-control-parameter d-l d-r (/ (- y-r y-l) (- x-r x-l))
                                                 prefer-shape-preservation)))

(defun convex-rational-cubic-control-parameter-to-fit-second-derivative-at-right-side
    (x-l x-r y-l y-r d-l d-r second-derivative-r prefer-shape-preservation)
  (max (rational-cubic-control-parameter-to-fit-second-derivative-at-right-side
        x-l x-r y-l y-r d-l d-r second-derivative-r)
       (minimum-rational-cubic-control-parameter d-l d-r (/ (- y-r y-l) (- x-r x-l))
                                                 prefer-shape-preservation)))

;;; --------------------------------------------------------------------
;;; The normalised Black function
;;; --------------------------------------------------------------------

(defun normalised-intrinsic (x q)
  "The normalised intrinsic value of a call (Q = 1) or put (Q = -1)."
  (declare (double-float x q))
  (if (<= (* q x) 0d0)
      0d0
      (let ((x2 (* x x)))
        (if (< x2 (* 98 +fourth-root-dbl-epsilon+))
            (abs (max (* (if (< q 0) -1d0 1d0) x
                         (+ 1 (* x2 (+ (/ 1d0 24d0)
                                       (* x2 (+ (/ 1d0 1920d0)
                                                (* x2 (+ (/ 1d0 322560d0)
                                                         (* (/ 1d0 92897280d0) x2)))))))))
                      0d0))
            (let* ((b-max (exp (* 0.5d0 x))) (one-over-b-max (/ b-max)))
              (abs (max (* (if (< q 0) -1d0 1d0) (- b-max one-over-b-max)) 0d0)))))))

(defun asymptotic-expansion-of-normalized-black-call (h tt)
  "b(h s, 2 t) for large negative (t - |h|): Abramowitz and Stegun 26.2.12,
to 17th order in q = (h/r)^2."
  (declare (double-float h tt))
  (let* ((e (* (/ tt h) (/ tt h)))
         (r (* (+ h tt) (- h tt)))
         (q (* (/ h r) (/ h r)))
         (asymptotic-expansion-sum
           ;; Horner in q, innermost level first; level k adds c_k(e) =
           ;; 2 (-1)^k sum_j C(2k+1, 2j+1) e^j and multiplies by (2k-1) q.
           (-> (horner e -70d0 -13090d0 -649264d0 -13449040d0 -141214920d0 -834451800d0
                         -2952675600d0 -6495886320d0 -9075135300d0 -8119857900d0 -4639918800d0
                         -1668903600d0 -367158792d0 -47071640d0 -3246320d0 -104720d0 -1190d0
                         -2d0)
               (* 33d0) (* q)
               (+ (horner e 66d0 10912d0 474672d0 8544096d0 77134200d0 387073440d0 1146332880d0
                            2074316640d0 2333606220d0 1637618400d0 709634640d0 185122080d0
                            27768312d0 2215136d0 81840d0 1056d0 2d0))
               (* (* 31d0 q))
               (+ (horner e -62d0 -8990d0 -339822d0 -5259150d0 -40320150d0 -169344630d0
                            -412506150d0 -601080390d0 -530365050d0 -282241050d0 -88704330d0
                            -15777450d0 -1472562d0 -62930d0 -930d0 -2d0))
               (* (* 29d0 q))
               (+ (horner e 58d0 7308d0 237510d0 3121560d0 20030010d0 69194580d0 135727830d0
                            155117520d0 103791870d0 40060020d0 8584290d0 950040d0 47502d0 812d0
                            2d0))
               (* (* 27d0 q))
               (+ (horner e -54d0 -5850d0 -161460d0 -1776060d0 -9373650d0 -26075790d0
                            -40116600d0 -34767720d0 -16872570d0 -4440150d0 -592020d0 -35100d0
                            -702d0 -2d0))
               (* (* 25d0 q))
               (+ (horner e 50d0 4600d0 106260d0 961400d0 4085950d0 8914800d0 10400600d0
                            6537520d0 2163150d0 354200d0 25300d0 600d0 2d0))
               (* (* 23d0 q))
               (+ (horner e -46d0 -3542d0 -67298d0 -490314d0 -1634380d0 -2704156d0 -2288132d0
                            -980628d0 -201894d0 -17710d0 -506d0 -2d0))
               (* (* 21d0 q))
               (+ (horner e 42d0 2660d0 40698d0 232560d0 587860d0 705432d0 406980d0 108528d0
                            11970d0 420d0 2d0))
               (* (* 19d0 q))
               (+ (horner e -38d0 -1938d0 -23256d0 -100776d0 -184756d0 -151164d0 -54264d0
                            -7752d0 -342d0 -2d0))
               (* (* 17d0 q))
               (+ (horner e 34d0 1360d0 12376d0 38896d0 48620d0 24752d0 4760d0 272d0 2d0))
               (* (* 15d0 q))
               (+ (horner e -30d0 -910d0 -6006d0 -12870d0 -10010d0 -2730d0 -210d0 -2d0))
               (* (* 13d0 q))
               (+ (horner e 26d0 572d0 2574d0 3432d0 1430d0 156d0 2d0))
               (* (* 11d0 q))
               (+ (horner e -22d0 -330d0 -924d0 -660d0 -110d0 -2d0))
               (* (* 9d0 q))
               (+ (horner e 18d0 168d0 252d0 72d0 2d0))
               (* (* 7d0 q))
               (+ (horner e -14d0 -70d0 -42d0 -2d0))
               (* (* 5d0 q))
               (+ (horner e 10d0 20d0 2d0))
               (* (* 3d0 q))
               (+ (horner e -6d0 -2d0))
               (* q)
               (+ 2d0)))
         (b (* +one-over-sqrt-two-pi+ (exp (* -0.5d0 (+ (* h h) (* tt tt))))
               (/ tt r) asymptotic-expansion-sum)))
    (declare (double-float e r q asymptotic-expansion-sum b))
    (abs (max b 0d0))))

(defun normalised-black-call-using-erfcx (h tt)
  "b = exp(-(h^2 + t^2)/2) [erfcx(-(h+t)/sqrt 2) - erfcx(-(h-t)/sqrt 2)] / 2:
one exponential instead of three, and the difference of two rational
functions."
  (declare (double-float h tt))
  (let ((b (* 0.5d0 (exp (* -0.5d0 (+ (* h h) (* tt tt))))
              (- (cody-calerf (* (- +one-over-sqrt-two+) (+ h tt)) 2)
                 (cody-calerf (* (- +one-over-sqrt-two+) (- h tt)) 2)))))
    (abs (max b 0d0))))

(defun small-t-expansion-of-normalized-black-call (h tt)
  "b for small t = s/2, to twelfth order in t; with a = 1 + h Y(h), Y = Phi/phi."
  (declare (double-float h tt))
  (let* ((a (+ 1 (* h (* 0.5d0 +sqrt-two-pi+)
                    (cody-calerf (* (- +one-over-sqrt-two+) h) 2))))
         (w (* tt tt))
         (h2 (* h h))
         (expansion
           ;; 2t (a + w (P1/6 + w (P2/120 + ...))), innermost level first; each
           ;; P_k is Horner in h2 with coefficients linear in a.
           (* 2d0 tt
              (-> (horner h2 (+ -89055d0 (* 135135d0 a)) (+ -82845d0 (* 270270d0 a))
                             (+ -20370d0 (* 135135d0 a)) (+ -1926d0 (* 25740d0 a))
                             (+ -75d0 (* 2145d0 a)) (+ -1d0 (* 78d0 a)) a)
                  (* w) (/ 6227020800d0)
                  (+ (/ (horner h2 (+ -6555d0 (* 10395d0 a)) (+ -4680d0 (* 17325d0 a))
                                   (+ -840d0 (* 6930d0 a)) (+ -52d0 (* 990d0 a))
                                   (+ -1d0 (* 55d0 a)) a)
                        39916800d0))
                  (* w)
                  (+ (/ (horner h2 (+ -561d0 (* 945d0 a)) (+ -285d0 (* 1260d0 a))
                                   (+ -33d0 (* 378d0 a)) (+ -1d0 (* 36d0 a)) a)
                        362880d0))
                  (* w)
                  (+ (/ (horner h2 (+ -57d0 (* 105d0 a)) (+ -18d0 (* 105d0 a))
                                   (+ -1d0 (* 21d0 a)) a)
                        5040d0))
                  (* w)
                  (+ (/ (horner h2 (+ -7d0 (* 15d0 a)) (+ -1d0 (* 10d0 a)) a)
                        120d0))
                  (* w)
                  (+ (/ (horner h2 (+ -1d0 (* 3d0 a)) a)
                        6d0))
                  (* w)
                  (+ a))))
         (b (* +one-over-sqrt-two-pi+ (exp (* -0.5d0 (+ (* h h) (* tt tt)))) expansion)))
    (declare (double-float a w h2 expansion b))
    (abs (max b 0d0))))

(defun normalized-black-call-using-norm-cdf (x s)
  (declare (double-float x s))
  (let* ((h (/ x s)) (tt (* 0.5d0 s)) (b-max (exp (* 0.5d0 x)))
         (b (- (* (lbr-norm-cdf (+ h tt)) b-max) (/ (lbr-norm-cdf (- h tt)) b-max))))
    (abs (max b 0d0))))

(defun normalised-black-call (x s)
  "The normalised Black call price b(x, s) = Phi(x/s + s/2) exp(x/2) -
Phi(x/s - s/2) exp(-x/2), accurate to a few ulps everywhere, by one of four
evaluations depending on the region.

  (normalised-black-call -1d0 0.5d0) => 0.0041423587506785...d0"
  (declare (double-float x s))
  (cond ((> x 0d0)
         (+ (normalised-intrinsic x 1d0) (normalised-black-call (- x) s)))
        ((<= s 0d0) (normalised-intrinsic x 1d0))
        ;; h < eta and t < tau + |h| - |eta|, without dividing by s.
        ((and (< x (* s +asymptotic-expansion-accuracy-threshold+))
              (< (+ (* 0.5d0 s s) x)
                 (* s (+ +small-t-expansion-of-normalized-black-threshold+
                         +asymptotic-expansion-accuracy-threshold+))))
         (asymptotic-expansion-of-normalized-black-call (/ x s) (* 0.5d0 s)))
        ((< (* 0.5d0 s) +small-t-expansion-of-normalized-black-threshold+)
         (small-t-expansion-of-normalized-black-call (/ x s) (* 0.5d0 s)))
        ;; When b is above about 85% of exp(x/2), its first term dominates.
        ((> (+ x (* 0.5d0 s s)) (* s 0.85d0))
         (normalized-black-call-using-norm-cdf x s))
        (t (normalised-black-call-using-erfcx (/ x s) (* 0.5d0 s)))))

(defun normalised-vega (x s)
  "db/ds."
  (declare (double-float x s))
  (let ((ax (abs x)))
    (cond ((<= ax 0d0) (* +one-over-sqrt-two-pi+ (exp (* -0.125d0 s s))))
          ((or (<= s 0d0) (<= s (* ax +sqrt-dbl-min+))) 0d0)
          (t (* +one-over-sqrt-two-pi+
                (exp (* -0.5d0 (+ (square (/ x s)) (square (* 0.5d0 s))))))))))

(defun lbr-black (forward strike sigma tau q)
  "The undiscounted Black price of a call (Q = 1) or put (Q = -1), accurate
to a few ulps also far out of the money, where BLACK-FORMULA's difference of
two terms loses digits.

  (lbr-black 100d0 110d0 0.2d0 1d0 1d0) => 4.29201094140988...d0"
  (declare (double-float forward strike sigma tau q))
  (let ((intrinsic (abs (max (if (< q 0) (- strike forward) (- forward strike)) 0d0))))
    (if (> (* q (- forward strike)) 0)
        ;; In the money: intrinsic plus the out-of-the-money option.
        (+ intrinsic (lbr-black forward strike sigma tau (- q)))
        (max intrinsic
             (* (* (sqrt forward) (sqrt strike))
                (normalised-black-call (if (< q 0) (- (log (/ forward strike)))
                                           (log (/ forward strike)))
                                       (* sigma (sqrt tau))))))))

;;; --------------------------------------------------------------------
;;; The initial-guess maps of the lowest and highest branches
;;; --------------------------------------------------------------------

(defun compute-f-lower-map-and-first-two-derivatives (x s)
  (declare (double-float x s))
  (let* ((ax (abs x)) (z (/ (* +sqrt-one-over-three+ ax) s)) (y (* z z)) (s2 (* s s))
         (phi-cdf (lbr-norm-cdf (- z))) (phi-pdf (lbr-norm-pdf z))
         ;; PI_OVER_SIX * y/(s2*s) * Phi * (...) * exp(...), left to right.
         (fpp (* (/ (* +pi-over-six+ y) (* s2 s)) phi-cdf
                 (+ (* 8 +sqrt-three+ s ax)
                    (/ (* (- (* 3 s2 (- s2 8)) (* 8 x x)) phi-cdf) phi-pdf))
                 (exp (+ (* 2 y) (* 0.25d0 s2)))))
         (phi2 (* phi-cdf phi-cdf))
         (fp (* +two-pi+ y phi2 (exp (+ y (* 0.125d0 s s)))))
         (f (* +two-pi-over-sqrt-twenty-seven+ ax (* phi2 phi-cdf))))
    (declare (double-float ax z y s2 phi-cdf phi-pdf fpp phi2 fp f))
    (values f fp fpp)))

(defun inverse-f-lower-map (x f)
  (declare (double-float x f))
  (abs (/ x (* +sqrt-three+
               (lbr-inverse-norm-cdf
                (expt (/ f (* +two-pi-over-sqrt-twenty-seven+ (abs x))) (/ 1d0 3d0)))))))

(defun compute-f-upper-map-and-first-two-derivatives (x s)
  (declare (double-float x s))
  (let* ((f (lbr-norm-cdf (* -0.5d0 s)))
         (w (square (/ x s)))
         (fp (* -0.5d0 (exp (* 0.5d0 w))))
         (fpp (/ (* +sqrt-pi-over-two+ (exp (+ w (* 0.125d0 s s))) w) s)))
    (declare (double-float f w fp fpp))
    (values f fp fpp)))

(defun inverse-f-upper-map (f)
  (declare (double-float f))
  (* -2d0 (lbr-inverse-norm-cdf f)))

(defun lbr-inverse-norm-cdf (u)
  "The normal quantile; the reference returns log(u) outside (0, 1)."
  (declare (double-float u))
  (cond ((<= u 0d0) (if (zerop u) (- most-positive-double-float) (log u)))
        ((>= u 1d0) (if (= u 1d0) most-positive-double-float (log (- 1 u))))
        (t (norm-quantile u))))

;;; --------------------------------------------------------------------
;;; The inversion
;;; --------------------------------------------------------------------

(declaim (inline householder-factor))
(defun householder-factor (newton halley hh3)
  (declare (double-float newton halley hh3))
  (/ (+ 1 (* 0.5d0 halley newton))
     (+ 1 (* newton (+ halley (/ (* hh3 newton) 6))))))

(defun unchecked-normalised-implied-volatility (beta x q n)
  "The normalised implied volatility s = sigma sqrt(T) of the normalised
price BETA at X = ln(F/K), for a call (Q = 1) or put (Q = -1), in at most N
Householder(3) steps. Returns 0 for BETA <= 0 and MOST-POSITIVE-DOUBLE-FLOAT
for BETA at or above the maximum, without other checks."
  (declare (double-float beta x q) (fixnum n))
  ;; Subtract the intrinsic value, then map puts to calls.
  (when (> (* q x) 0d0)
    (setf beta (abs (max (- beta (normalised-intrinsic x q)) 0d0))
          q (- q)))
  (when (< q 0d0)
    (setf x (- x) q (- q)))
  (when (<= beta 0d0)
    (return-from unchecked-normalised-implied-volatility 0d0))
  (let ((b-max (exp (* 0.5d0 x))))
    (declare (double-float b-max))
    (when (>= beta b-max)
      (return-from unchecked-normalised-implied-volatility most-positive-double-float))
    (let* ((iterations 0) (direction-reversal-count 0)
           (f (- most-positive-double-float)) (s (- most-positive-double-float))
           (ds s) (ds-previous 0d0)
           (s-left least-positive-normalized-double-float) (s-right most-positive-double-float)
           (s-c (sqrt (abs (* 2 x))))
           (b-c (normalised-black-call x s-c))
           (v-c (normalised-vega x s-c)))
      (declare (fixnum iterations direction-reversal-count)
               (double-float f s ds ds-previous s-left s-right s-c b-c v-c))
      (macrolet ((householder-loop (&body step)
                   ;; The reference's iteration with its safeguards: binary
                   ;; nesting when the step leaves the bracket or the direction
                   ;; reverses three times. STEP computes DS from B and BP.
                   `(loop while (and (< iterations n) (> (abs ds) (* +dbl-epsilon+ s)))
                          do (when (< (* ds ds-previous) 0) (incf direction-reversal-count))
                             (when (and (> iterations 0)
                                        (or (= 3 direction-reversal-count)
                                            (not (and (> s s-left) (< s s-right)))))
                               (setf s (* 0.5d0 (+ s-left s-right)))
                               (when (<= (- s-right s-left) (* +dbl-epsilon+ s)) (return))
                               (setf direction-reversal-count 0 ds 0d0))
                             (setf ds-previous ds)
                             (let ((b (normalised-black-call x s)) (bp (normalised-vega x s)))
                               (declare (double-float b bp))
                               (cond ((and (> b beta) (< s s-right)) (setf s-right s))
                                     ((and (< b beta) (> s s-left)) (setf s-left s)))
                               ,@step)
                             (setf ds (max (* -0.5d0 s) ds))
                             (incf s ds)
                             (incf iterations))))
        (if (< beta b-c)
            (let* ((s-l (- s-c (/ b-c v-c))) (b-l (normalised-black-call x s-l)))
              (declare (double-float s-l b-l))
              (if (< beta b-l)
                  ;; The lowest branch: g(s) = 1/ln(b(s)) - 1/ln(beta).
                  (multiple-value-bind (f-lower-map-l d-f-lower-map-l-d-beta d2-f-lower-map-l-d-beta2)
                      (compute-f-lower-map-and-first-two-derivatives x s-l)
                    (declare (double-float f-lower-map-l d-f-lower-map-l-d-beta
                                           d2-f-lower-map-l-d-beta2))
                    (let ((r-ll (convex-rational-cubic-control-parameter-to-fit-second-derivative-at-right-side
                                 0d0 b-l 0d0 f-lower-map-l 1d0 d-f-lower-map-l-d-beta
                                 d2-f-lower-map-l-d-beta2 t)))
                      (setf f (rational-cubic-interpolation beta 0d0 b-l 0d0 f-lower-map-l 1d0
                                                            d-f-lower-map-l-d-beta r-ll))
                      (unless (> f 0d0)
                        ;; Round-off for extreme |x|: quadratic interpolation
                        ;; with f(0) = 0, f(b_l) and f'(0) = 1.
                        (let ((tt (/ beta b-l)))
                          (setf f (* (+ (* f-lower-map-l tt) (* b-l (- 1 tt))) tt))))
                      (setf s (inverse-f-lower-map x f)
                            s-right s-l)
                      (householder-loop
                       (if (or (<= b 0d0) (<= bp 0d0))
                           (setf ds (- (* 0.5d0 (+ s-left s-right)) s))
                           (let* ((ln-b (log b)) (ln-beta (log beta)) (bpob (/ bp b))
                                  (h (/ x s)) (b-halley (- (/ (* h h) s) (/ s 4)))
                                  (newton (/ (/ (* (- ln-beta ln-b) ln-b) ln-beta) bpob))
                                  (halley (- b-halley (* bpob (+ 1 (/ 2 ln-b)))))
                                  (b-hh3 (- (* b-halley b-halley) (* 3 (square (/ h s))) 0.25d0))
                                  (hh3 (- (+ b-hh3 (* 2 (square bpob)
                                                       (+ 1 (* (/ 3 ln-b) (+ 1 (/ 1 ln-b))))))
                                          (* 3 b-halley bpob (+ 1 (/ 2 ln-b))))))
                             (declare (double-float ln-b ln-beta bpob h b-halley newton halley
                                                    b-hh3 hh3))
                             (setf ds (* newton (householder-factor newton halley hh3))))))
                      (return-from unchecked-normalised-implied-volatility s)))
                  ;; The lower middle branch.
                  (let* ((v-l (normalised-vega x s-l))
                         (r-lm (convex-rational-cubic-control-parameter-to-fit-second-derivative-at-right-side
                                b-l b-c s-l s-c (/ v-l) (/ v-c) 0d0 nil)))
                    (setf s (rational-cubic-interpolation beta b-l b-c s-l s-c (/ v-l) (/ v-c) r-lm)
                          s-left s-l
                          s-right s-c))))
            (let* ((s-h (if (> v-c least-positive-normalized-double-float)
                            (+ s-c (/ (- b-max b-c) v-c))
                            s-c))
                   (b-h (normalised-black-call x s-h)))
              (declare (double-float s-h b-h))
              (if (<= beta b-h)
                  ;; The upper middle branch.
                  (let* ((v-h (normalised-vega x s-h))
                         (r-hm (convex-rational-cubic-control-parameter-to-fit-second-derivative-at-left-side
                                b-c b-h s-c s-h (/ v-c) (/ v-h) 0d0 nil)))
                    (setf s (rational-cubic-interpolation beta b-c b-h s-c s-h (/ v-c) (/ v-h) r-hm)
                          s-left s-c
                          s-right s-h))
                  ;; The highest branch: g(s) = ln((b_max - beta) / (b_max - b(s))).
                  (multiple-value-bind (f-upper-map-h d-f-upper-map-h-d-beta d2-f-upper-map-h-d-beta2)
                      (compute-f-upper-map-and-first-two-derivatives x s-h)
                    (declare (double-float f-upper-map-h d-f-upper-map-h-d-beta
                                           d2-f-upper-map-h-d-beta2))
                    (when (and (> d2-f-upper-map-h-d-beta2 (- +sqrt-dbl-max+))
                               (< d2-f-upper-map-h-d-beta2 +sqrt-dbl-max+))
                      (let ((r-hh (convex-rational-cubic-control-parameter-to-fit-second-derivative-at-left-side
                                   b-h b-max f-upper-map-h 0d0 d-f-upper-map-h-d-beta -0.5d0
                                   d2-f-upper-map-h-d-beta2 t)))
                        (setf f (rational-cubic-interpolation beta b-h b-max f-upper-map-h 0d0
                                                              d-f-upper-map-h-d-beta -0.5d0 r-hh))))
                    (when (<= f 0d0)
                      ;; Quadratic interpolation with f(b_h), f(b_max) = 0 and
                      ;; f'(b_max) = -1/2.
                      (let* ((h (- b-max b-h)) (tt (/ (- beta b-h) h)))
                        (setf f (* (+ (* f-upper-map-h (- 1 tt)) (* 0.5d0 h tt)) (- 1 tt)))))
                    (setf s (inverse-f-upper-map f)
                          s-left s-h)
                    ;; Below half of b_max the plain objective b(s) - beta is used.
                    (when (> beta (* 0.5d0 b-max))
                      (householder-loop
                       (if (or (>= b b-max) (<= bp least-positive-normalized-double-float))
                           (setf ds (- (* 0.5d0 (+ s-left s-right)) s))
                           (let* ((b-max-minus-b (- b-max b))
                                  (g (log (/ (- b-max beta) b-max-minus-b)))
                                  (gp (/ bp b-max-minus-b))
                                  (b-halley (- (/ (square (/ x s)) s) (/ s 4)))
                                  (b-hh3 (- (* b-halley b-halley) (* 3 (square (/ x (* s s)))) 0.25d0))
                                  (newton (/ (- g) gp))
                                  (halley (+ b-halley gp))
                                  (hh3 (+ b-hh3 (* gp (+ (* 2 gp) (* 3 b-halley))))))
                             (declare (double-float b-max-minus-b g gp b-halley b-hh3 newton
                                                    halley hh3))
                             (setf ds (* newton (householder-factor newton halley hh3))))))
                      (return-from unchecked-normalised-implied-volatility s))))))
        ;; The two middle branches (and the upper one below b_max/2): the
        ;; objective is g(s) = b(s) - beta.
        (householder-loop
         (let* ((newton (/ (- beta b) bp))
                (halley (- (/ (square (/ x s)) s) (/ s 4)))
                (hh3 (- (* halley halley) (* 3 (square (/ x (* s s)))) 0.25d0)))
           (declare (double-float newton halley hh3))
           (setf ds (* newton (householder-factor newton halley hh3)))))
        s))))

(defun lbr-implied-volatility (price forward strike tau q)
  "The Black volatility of the undiscounted PRICE of a call (Q = 1) or put
(Q = -1). Returns MOST-NEGATIVE-DOUBLE-FLOAT below the intrinsic value and
MOST-POSITIVE-DOUBLE-FLOAT at or above the maximum (F for a call, K for a
put), as the reference does; callers check bounds first.

  (lbr-implied-volatility 4.29201094140988d0 100d0 110d0 1d0 1d0) => 0.2d0"
  (declare (double-float price forward strike tau q))
  (let ((intrinsic (abs (max (if (< q 0) (- strike forward) (- forward strike)) 0d0))))
    (cond ((< price intrinsic) most-negative-double-float)
          ((>= price (if (< q 0) strike forward)) most-positive-double-float)
          (t
           (let ((x (log (/ forward strike))))
             ;; Map in-the-money to out-of-the-money.
             (when (> (* q x) 0d0)
               (setf price (abs (max (- price intrinsic) 0d0))
                     q (- q)))
             (/ (unchecked-normalised-implied-volatility
                 (/ price (* (sqrt forward) (sqrt strike))) x q +lbr-maximum-iterations+)
                (sqrt tau)))))))
