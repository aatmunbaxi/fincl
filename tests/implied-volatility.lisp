;;;; implied-volatility.lisp --- Black and Bachelier inversions, from any engine

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun lbr-fixture ()
  (with-open-file (s (asdf:system-relative-pathname "fincl" "tests/fixtures/lets-be-rational.sexp"))
    (let ((*read-default-float-format* 'double-float))
      (loop for entry = (read s nil s) until (eq entry s) collect entry))))

(defun relative-difference (a b)
  (if (= a b) 0d0 (abs (/ (- a b) (if (zerop b) 1d0 b)))))

(defun lbr-reference-checks ()
  (format t "~&Let's Be Rational against Jäckel's reference C++~%")
  (let ((worst (make-hash-table)) (identical (make-hash-table)) (count (make-hash-table)))
    (loop for (op args expected) in (lbr-fixture)
          for got = (ecase op
                      (:nbc (apply #'fincl::normalised-black-call args))
                      (:erfcx (erfcx (first args)))
                      (:ncdf (fincl::lbr-norm-cdf (first args)))
                      (:black (apply #'fincl::lbr-black args))
                      (:iv (apply #'fincl::lbr-implied-volatility args)))
          do (incf (gethash op count 0))
             (when (= got expected) (incf (gethash op identical 0)))
             (setf (gethash op worst) (max (gethash op worst 0d0)
                                           (relative-difference got expected))))
    (dolist (op '(:nbc :erfcx :ncdf :black))
      (check-that (format nil "~(~A~): ~D of ~D bit-identical to the reference" op
                          (gethash op identical 0) (gethash op count))
                  (= (gethash op identical 0) (gethash op count))))
    ;; The inversion's initial guess uses NORM-QUANTILE (AS241 plus a Halley
    ;; step), more accurate than the reference's AS241 alone.
    (check-that (format nil "implied vols within 2e-15 of the reference (worst ~,1E)"
                        (gethash :iv worst))
                (< (gethash :iv worst) 2d-15))))

(defun black-vega (forward strike sigma tau)
  "Undiscounted Black vega dP/dsigma."
  (let ((d1 (/ (+ (log (/ forward strike)) (* 0.5d0 sigma sigma tau)) (* sigma (sqrt tau)))))
    (* forward (norm-pdf d1) (sqrt tau))))

(defun attainable-error (price sigma vega)
  "The relative error in sigma that one ulp of PRICE implies."
  (/ (* double-float-epsilon 2 price) (* sigma vega)))

(defun implied-vol-checks ()
  (format t "~&Implied Black volatility (Let's Be Rational)~%")
  (let ((cases 0) (well-conditioned 0) (worst-well 0d0) (worst-ratio 0d0) (clamped 0) (at-limit 0)
        (seconds 0d0) (discount 0.9d0) (forward 100d0))
    (dolist (sigma '(0.005d0 0.02d0 0.1d0 0.3d0 1d0 3d0))
      (dolist (tau `(,(/ 1d0 365) 0.1d0 1d0 5d0 30d0))
        (dolist (z '(-8d0 -4d0 -1d0 -0.1d0 0d0 0.1d0 1d0 4d0 8d0))
          (dolist (phi '(1d0 -1d0))
            (let* ((strike (* forward (exp (- (* z sigma (sqrt tau))))))
                   (undiscounted (fincl::lbr-black forward strike sigma tau phi))
                   (price (* discount undiscounted))
                   (start (get-internal-real-time))
                   (implied (handler-case (implied-black-vol phi price forward strike discount tau)
                              (price-out-of-bounds () :out-of-bounds)))
                   (error (if (numberp implied) (relative-difference implied sigma) 0d0))
                   (attainable (attainable-error undiscounted sigma
                                                 (black-vega forward strike sigma tau))))
              (incf seconds (/ (- (get-internal-real-time) start)
                               (float internal-time-units-per-second 1d0)))
              (incf cases)
              (cond ((eq implied :out-of-bounds)
                     ;; Signalled: correct only if the price rounded to its limit.
                     (incf at-limit)
                     (unless (>= undiscounted (if (plusp phi) forward strike))
                       (incf *failures*)
                       (format t "~&  FAIL ~A: out of bounds below the limit~%"
                               `(:sigma ,sigma :tau ,tau :z ,z :phi ,phi))))
                    ((zerop implied)
                     ;; The time value rounded away: the price is the
                     ;; intrinsic value, and 0 is the documented answer.
                     (incf clamped)
                     (unless (= undiscounted (max 0d0 (* phi (- forward strike))))
                       (incf *failures*)
                       (format t "~&  FAIL ~A: vol 0 for a price above intrinsic~%"
                               `(:sigma ,sigma :tau ,tau :z ,z :phi ,phi))))
                    ((< attainable 1d-14)
                     (incf well-conditioned)
                     (setf worst-well (max worst-well error)))
                    (t (setf worst-ratio (max worst-ratio (/ error attainable))))))))))
    (check-that (format nil "~D well-conditioned cases (one ulp of price moves sigma by < 1e-14): ~
|sigma^ - sigma| / sigma < 1e-12 (worst ~,1E)" well-conditioned worst-well)
                (< worst-well 1d-12))
    (check-that (format nil "the other ~D: within 4x the attainable precision (worst ~,2Fx); ~
~D priced at intrinsic give 0, ~D priced at the limit signal"
                        (- cases well-conditioned clamped at-limit) worst-ratio clamped at-limit)
                (<= worst-ratio 4d0))
    ;; Timing, over a fresh loop so the GC and compilation are out of it.
    (let* ((inputs (loop for i below 20000
                         for sigma = (+ 0.05d0 (* 0.9d0 (/ i 20000d0)))
                         for strike = (+ 60d0 (* 80d0 (/ (mod (* i 7919) 20000) 20000d0)))
                         collect `(,(* 0.95d0 (fincl::lbr-black 100d0 strike sigma 1d0 1d0)) ,strike)))
           (start (get-internal-real-time)))
      (loop for (price strike) in inputs
            do (implied-black-vol 1d0 price 100d0 strike 0.95d0 1d0))
      (let ((micros (/ (* 1d6 (- (get-internal-real-time) start))
                       internal-time-units-per-second (length inputs))))
        (check-that (format nil "mean time per inversion ~,2F us (target < 5)" micros)
                    (< micros 5d0)))))

  (format t "~&Bounds~%")
  (check-error "a call price at the discounted forward"
               (implied-black-vol 1 95d0 100d0 90d0 0.95d0 1d0) price-out-of-bounds)
  (check-error "a put price below intrinsic"
               (implied-black-vol -1 9d0 100d0 110d0 1d0 1d0) price-out-of-bounds)
  (check "a price at intrinsic has vol 0" 0d0 (implied-black-vol -1 10d0 100d0 110d0 1d0 1d0) 0)
  (check "use-value returns the given vol" 0.25d0
         (handler-bind ((price-out-of-bounds (lambda (c) (use-value 0.25d0 c))))
           (implied-black-vol 1 200d0 100d0 90d0 1d0 1d0))
         0)
  (check-that "the report names the price and the bounds"
              (search "95.0 has no black implied volatility: it must lie in [0.0, 90.0)"
                      (error-message (lambda ()
                                                           (implied-black-vol -1 95d0 100d0 90d0 1d0 1d0)))))

  (format t "~&Implied normal (Bachelier) volatility~%")
  (let ((worst-well 0d0) (well 0) (worst-ratio 0d0) (cases 0) (discount 0.97d0))
    (dolist (forward '(-0.01d0 0.002d0 0.03d0 100d0))
      (dolist (sigma '(1d-4 0.005d0 0.02d0 1d0 30d0))
        (dolist (tau `(,(/ 1d0 365) 1d0 30d0))
          (dolist (z '(-8d0 -4d0 -1d0 -0.1d0 0d0 0.1d0 1d0 4d0 8d0))
            (dolist (phi '(1d0 -1d0))
              (let* ((stddev (* sigma (sqrt tau)))
                     (strike (- forward (* z stddev)))
                     (price (bachelier-formula phi forward strike discount (* stddev stddev)))
                     (implied (implied-normal-vol phi price forward strike discount tau))
                     (error (relative-difference implied sigma))
                     (vega (* discount (sqrt tau) (norm-pdf z)))
                     (attainable (attainable-error price sigma vega)))
                (incf cases)
                (if (< attainable 1d-14)
                    (progn (incf well) (setf worst-well (max worst-well error)))
                    (setf worst-ratio (max worst-ratio (/ error attainable))))))))))
    (check-that (format nil "~D well-conditioned cases, negative forwards and strikes included: ~
relative error < 1e-12 (worst ~,1E)" well worst-well)
                (< worst-well 1d-12))
    (check-that (format nil "the other ~D: within 4x the attainable precision (worst ~,2Fx)"
                        (- cases well) worst-ratio)
                (<= worst-ratio 4d0)))
  (check "Bachelier at the money: sigma sqrt(T) / sqrt(2 pi), discounted"
         (* 0.9d0 0.01d0 (sqrt 2d0) (/ (sqrt (* 2 pi))))
         (bachelier-formula 1 0.03d0 0.03d0 0.9d0 (* 0.01d0 0.01d0 2))
         1d-18)
  (check "Bachelier put-call parity: C - P = D (F - K)" (* 0.9d0 (- -0.01d0 0.004d0))
         (- (bachelier-formula 1 -0.01d0 0.004d0 0.9d0 1d-4)
            (bachelier-formula -1 -0.01d0 0.004d0 0.9d0 1d-4))
         1d-17)
  (check-error "a normal price below intrinsic"
               (implied-normal-vol 1 0.001d0 0.03d0 0.02d0 1d0 1d0) price-out-of-bounds))

(defun engine-implied-vol-checks ()
  (format t "~&Implied volatility from any engine~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :dividend 0.02d0
                            :vol 0.3d0))
         (aput (option (put 105) (american) (expires #D"2027-09-24")))
         (baw (make-instance 'barone-adesi-whaley-engine))
         (tree (binomial 1000)))
    (dolist (engine (list baw tree))
      (check (format nil "~(~A~): an American put priced at 0.3 implies 0.3"
                     (class-name (class-of engine)))
             0.3d0 (implied-volatility aput (price aput engine m) engine m) 1d-8))
    (check "a European through IMPLIED-VOLATILITY is IMPLIED-BLACK-VOL, whatever the engine"
           0.3d0
           (let ((call (option (call 110) (expires #D"2027-09-24"))))
             (implied-volatility call (price call (make-instance 'black-scholes-engine) m)
                                 (mc :n-paths 1000) m))
           1d-14)
    (check-error "an American put at its intrinsic value (many vols reproduce it)"
                 (implied-volatility (option (put 200) (american) (expires #D"2027-09-24"))
                                     100d0 baw m)
                 price-out-of-bounds)
    (check-error "a price above the bracket's high end"
                 (implied-volatility aput 104d0 baw m)
                 price-out-of-bounds)))
