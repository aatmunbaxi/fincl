(defclass european-option (option) ())

;; (defmethod price ((op european-option)
;;                   spot
;;                   (engine pricing-engine)
;;                   (process 'GBM))
;;   (cond ((equal engine 'analytic)
;;          )))


(defmethod analytic-option-pricer ((op european-option)
                                   spot
                                   process)
  (flet ((cdf (x) (/  (special-functions:erfc  (coerce (/ (- 0 x) (sqrt 2)) 'double-float)) 2)))
    (let* ((tau (tte op))
           (sig (sigma process))
           (discount (exp (* -0.039 tau)))
           (d+ (*
                (/ 1 (* sig (sqrt tau)))
                (+ (log (/ spot (strike op))) (+ 0.039 (* (/ (expt sig 2) 2) tau)))))
           (d- (- d+ (* sig (sqrt tau)))))
      (cond ((eql 'call (callput op))
             (- (* (cdf d+) spot) (* (cdf d-) (strike op) discount)))
            ((eql 'put (callput op))
             (- (* (cdf (- 0  d-)) spot discount) (* (cdf (- 0 d+)) (strike op))))))))


(defmethod binomial-option-pricer ((op european-option)
                                   spot trials up down)
  (let* ((n trials)
         (is (magicl:arange (1+ trials) :type 'double-float))
         (rfr (1+ 0.039))
         (p (/ (- rfr down) (- up down))))


    (/ (magicl:sum (magicl:map! (lambda (j)
                                  (* (alexandria:binomial-coefficient
                                      trials
                                      (coerce (round j) 'unsigned-byte))
                                     (expt p j)
                                     (expt (- 1 p) (- n j))
                                     (max
                                      0
                                      (- (* (expt up j)
                                            (expt down (- n j)) spot)
                                         (strike op)))))
                                is))
       (expt rfr n))))



(defmethod mc-option-pricer ((op european-option)
                             spot num-sims num-steps process)
  (flet ((colmean (x) (/ (magicl:sum x) (car (magicl:shape x)))))
    (let* ((paths (gen-paths process spot (tte op) num-sims num-steps))
           (final-value (magicl:column paths num-steps)))
      ;; (format t "~a"   final-value)
      (cond ((equal 'call (callput op))
             (* (exp (* -0.039 (tte op)))
                (colmean
                 (magicl:.max 0 (magicl:.- final-value (strike op))))))
            ((equal 'put (callput op))
             (colmean (magicl:.* (exp (* -0.039 (tte op)))
                                 (magicl:.max 0 (magicl:.- (strike op) final-value)))))))))
