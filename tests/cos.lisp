;;;; cos.lisp --- the Fourier-cosine engine

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun cos-engine (&rest initargs)
  (apply #'make-instance 'cos-engine initargs))

(defun direct-continuation-coefficients (cf-samples coeffs x1 x2 a b discount)
  "O(N^2) reference for FINCL::COS-CONTINUATION-COEFFICIENTS: the matrix
product C_k = DISCOUNT Re sum_j cf_j V_j (I(j+k) + I(j-k)) / (b - a)."
  (let* ((n (length coeffs))
         (table (fincl::cos-integral-table n x1 x2 a b))
         (out (make-array n :element-type 'double-float)))
    (dotimes (k n out)
      (setf (aref out k)
            (* (/ discount (- b a))
               (iter (for j below n)
                     (sum (realpart (* (aref cf-samples j) (aref coeffs j)
                                       (+ (aref table (+ j k n -1))
                                          (aref table (+ (- j k) n -1))))))))))))

(defun naive-dft (v)
  (let* ((n (length v))
         (out (make-array n :element-type '(complex double-float))))
    (dotimes (k n out)
      (setf (aref out k)
            (iter (for j below n)
                  (sum (* (aref v j) (cis (/ (* -2 pi j k) n)))))))))

(defun fft-checks ()
  (format t "~&COS: FFT continuation step~%")
  (let ((v (make-array 64 :element-type '(complex double-float))))
    (dotimes (i 64)
      (setf (aref v i) (complex (sin (* 1.3d0 i)) (cos (* 0.7d0 i i)))))
    (let ((expected (naive-dft v))
          (got (fincl::fft! (copy-seq v))))
      (check "FFT = naive DFT, max error" 0
             (iter (for i below 64) (maximize (abs (- (aref expected i) (aref got i)))))
             1d-12))
    (check "inverse FFT round trip, max error" 0
           (let ((w (fincl::fft! (fincl::fft! (copy-seq v)) :inverse t)))
             (iter (for i below 64) (maximize (abs (- (/ (aref w i) 64) (aref v i))))))
           1d-14))
  ;; A GBM put's terminal coefficients propagated one step back, over a
  ;; window that cuts through the range, by FFT and by the direct product.
  (dolist (n '(32 100 256))
    (let* ((a -2d0) (b 1.5d0)
           (cf (characteristic-function (make-instance 'gbm)
                                        (make-bs-market :valuation-date *today* :spot 100
                                                        :rate 0.05d0 :vol 0.25d0)
                                        0.1d0))
           (samples (fincl::cos-cf-samples cf n (/ pi (- b a))))
           (coeffs (fincl::cos-payoff-coefficients -1d0 100d0 n a b a 0d0))
           (fft (fincl::cos-continuation-coefficients samples coeffs -0.3d0 b a b 0.99d0))
           (direct (direct-continuation-coefficients samples coeffs -0.3d0 b a b 0.99d0)))
      (check (format nil "continuation coefficients, FFT = direct, N = ~D" n) 0
             (iter (for k below n) (maximize (abs (- (aref fft k) (aref direct k)))))
             1d-11))))

(defun cos-checks ()
  (let* ((bs (make-instance 'black-scholes-engine))
         (m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                            :dividend 0.03d0 :vol 0.25d0)))

    (format t "~&COS: European under GBM~%")
    (dolist (clauses '(((:call 80) (:expires #D"2027-09-24")) ((:call 100) (:expires #D"2027-09-24"))
                       ((:call 120) (:expires #D"2027-09-24")) ((:put 80) (:expires #D"2027-03-10"))
                       ((:put 100) (:expires #D"2027-03-10")) ((:put 120) (:expires #D"2028-09-22"))))
      (let ((o (make-option-from-clauses clauses)))
        (check (format nil "~A = Black-Scholes" o)
               (price o bs m) (price o (cos-engine :n-terms 128) m) 1d-11)))
    ;; Fang and Oosterlee (2008), section 5.1: short-dated calls converge
    ;; exponentially in N.
    (let* ((short (make-bs-market :valuation-date *today* :spot 100 :rate 0.1d0 :vol 0.25d0))
           (o (make-option (call 100) (expires (date+ *today* 37))))
           (exact (price o bs short))
           (errors (iter (for n in '(16 32 64))
                         (collect (abs (- exact (price o (cos-engine :n-terms n :truncation 10d0)
                                                       short)))))))
      (check-that (format nil "exponential convergence, errors at N = 16, 32, 64: ~{~,1E~^, ~}"
                          errors)
                  (destructuring-bind (e16 e32 e64) errors
                    (and (< e32 (* 1d-4 e16)) (< e64 1d-12)))))

    (format t "~&COS: European under Heston~%")
    ;; Fang and Oosterlee (2008), section 5.2, reference 5.785155450.
    (let ((flat (make-bs-market :valuation-date *today* :spot 100 :rate 0 :vol 0.2d0))
          (heston (make-instance 'heston :kappa 1.5768d0 :xi 0.5751d0 :theta 0.0398d0
                                         :v0 0.0175d0 :rho -0.5711d0)))
      (check "Heston call, paper parameters" 5.785155450d0
             (price (make-option (call 100) (expires #D"2027-09-24"))
                    (cos-engine :process heston :n-terms 256) flat)
             1d-6)
      (check "Heston put by parity at the forward: call = put" 0
             (- (price (make-option (call 100) (expires #D"2027-09-24"))
                       (cos-engine :process heston) flat)
                (price (make-option (put 100) (expires #D"2027-09-24"))
                       (cos-engine :process heston) flat))
             1d-12))
    (let ((m5 (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
      (check "Heston xi=0 == BS" 10.4506
             (price (make-option (call 100) (expires #D"2027-09-24"))
                    (cos-engine :process (make-instance 'heston :v0 0.04d0 :theta 0.04d0
                                                                :xi 0d0))
                    m5)
             1d-4)
      (check "Heston xi=0 == BS, exactly"
             (price (make-option (call 100) (expires #D"2027-09-24")) bs m5)
             (price (make-option (call 100) (expires #D"2027-09-24"))
                    (cos-engine :process (make-instance 'heston :v0 0.04d0 :theta 0.04d0
                                                                :xi 0d0))
                    m5)
             1d-10))

    (fft-checks)

    (format t "~&COS: early exercise under GBM~%")
    (let* ((ls (make-bs-market :valuation-date *today* :spot 36 :rate 0.06d0 :vol 0.2d0))
           (aput (make-option (put 40) (american) (expires #D"2027-09-24")))
           (reference (price aput (binomial 8000) ls)))
      (check (format nil "American put = binomial (~,6F)" reference)
             reference (price aput (cos-engine) ls) 2d-5)
      (check "American put, Longstaff-Schwartz benchmark 4.478" 4.478
             (price aput (cos-engine) ls) 0.01))
    (let ((reference (price (make-option (call 100) (american) (expires #D"2027-09-24"))
                          (binomial 8000) m)))
      (check (format nil "American call with dividends = binomial (~,6F)" reference)
             reference
             (price (make-option (call 100) (american) (expires #D"2027-09-24")) (cos-engine) m)
             2d-5))
    (let ((no-div (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.25d0)))
      (check "American call without dividends = European"
             (price (make-option (call 100) (expires #D"2027-09-24")) bs no-div)
             (price (make-option (call 100) (american) (expires #D"2027-09-24")) (cos-engine) no-div)
             1d-5))
    (let* ((payoff (make-instance 'vanilla-payoff :kind :put :strike 100d0))
           (bermudan (lambda (dates)
                       (make-instance 'option :payoff payoff
                                              :exercise (make-instance 'bermudan-exercise
                                                                       :dates dates))))
           (european (price (make-option (put 100) (expires #D"2027-09-24")) bs m))
           (american (price (make-option (put 100) (american) (expires #D"2027-09-24")) (cos-engine) m)))
      (check "Bermudan with one date = European" european
             (price (funcall bermudan (list #D"2027-09-24")) (cos-engine) m) 1d-10)
      (check-that "European < quarterly Bermudan < American"
                  (< european
                     (price (funcall bermudan (list #D"2026-12-24" #D"2027-03-24"
                                                    #D"2027-06-24" #D"2027-09-24"))
                            (cos-engine) m)
                     american)))

    (format t "~&COS: conventions and unsupported combinations~%")
    (let* ((scale (/ 360d0 365d0))
           (m360 (make-bs-market :valuation-date *today* :day-counter +actual-360+
                                 :spot 100 :rate (* 0.05d0 scale) :dividend (* 0.03d0 scale)
                                 :vol (* 0.25d0 (sqrt scale)))))
      (dolist (clauses '(((:call 100) (:expires #D"2027-06-18"))
                         ((:put 100) (:american) (:expires #D"2027-06-18"))))
        (let ((o (make-option-from-clauses clauses)))
          (check (format nil "~A: ACT/365F = rescaled ACT/360" o)
                 (price o (cos-engine) m) (price o (cos-engine) m360) 1d-10))))
    (check-error "European under CEV (no characteristic function)"
                 (price (make-option (call 100) (expires #D"2027-09-24"))
                        (cos-engine :process (make-instance 'cev :beta 0.5d0)) m)
                 unsupported-combination)
    (check-that "unsupported report names the process"
                (search "CEV" (error-message
                               (lambda ()
                                 (price (make-option (call 100) (expires #D"2027-09-24"))
                                        (cos-engine :process (make-instance 'cev :beta 0.5d0))
                                        m)))))
    (check-error "American under Heston"
                 (price (make-option (put 100) (american) (expires #D"2027-09-24"))
                        (cos-engine :process (make-instance 'heston)) m)
                 unsupported-combination)))

(defun run-cos-tests ()
  "Run only the COS engine checks. Returns T when all pass."
  (let ((*failures* 0))
    (cos-checks)
    (cos-term-structure-checks)
    (format t "~&~[All COS checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))

;;; --------------------------------------------------------------------
;;; Term structure (task 08)
;;; --------------------------------------------------------------------

(defun cos-term-structure-checks ()
  (format t "~&COS: term structure~%")
  (let* ((pm (make-piecewise-market))
         ;; Piecewise rates and dividends, flat vol: the model a CRR tree,
         ;; which has one vol, shares with COS.
         (pm-flat-vol (make-piecewise-market :vols '(0.2d0 0.2d0 0.2d0 0.2d0)))
         (cos (make-instance 'cos-engine))
         (quarterly (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24")))
    (flet ((bermudan (kind strike dates)
             (make-instance 'option
                            :payoff (make-instance 'vanilla-payoff :kind kind :strike strike)
                            :exercise (make-instance 'bermudan-exercise :dates dates))))
      (dolist (kind '(:call :put))
        (let ((phi (if (eq kind :call) 1d0 -1d0)))
          (check (format nil "single-date Bermudan ~(~A~) = Black, vol and rate term structure" kind)
                 (fincl::black-formula phi (forward pm 1d0) 100d0 (discount-factor pm 1d0)
                                       (black-variance pm 100d0 1d0))
                 (price (bermudan kind 100d0 (list #D"2027-09-24")) cos pm)
                 1d-10)
          ;; 7300 steps over 365 days: every date on the grid for N and N/2.
          (check (format nil "quarterly Bermudan ~(~A~) = BBSR binomial, rate term structure" kind)
                 (price (bermudan kind 100d0 quarterly) (binomial 7300) pm-flat-vol)
                 (price (bermudan kind 100d0 quarterly) cos pm-flat-vol)
                 1d-4)
          (let ((american (make-option-from-clauses `((,kind 100) (:american) (:expires #D"2027-09-24")))))
            (check (format nil "American ~(~A~) = BBSR binomial, rate term structure" kind)
                   (price american (binomial 8000) pm-flat-vol)
                   (price american cos pm-flat-vol)
                   1d-3))))
      (check-that "Bermudan under a vol term structure lies between European and American"
                  (let ((european (price (make-option (put 100) (expires #D"2027-09-24")) cos pm))
                        (bermudan (price (bermudan :put 100d0 quarterly) cos pm))
                        (american (price (make-option (put 100) (american) (expires #D"2027-09-24")) cos pm)))
                    (< european bermudan american))))))
