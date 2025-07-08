(defclass european-option (option) ())


(defmethod mc-pricer ((op european-option)
                      spot sims steps process)
  (flet ((colmean (x) (/ (magicl:sum x) (car (magicl:shape x)))))
    (let* ((paths (gen-paths process spot (tte op) sims steps))
           (final-value (magicl:column paths (1- steps))))
      (cond ((equal 'call (callput op))
             (progn
               (colmean
                (magicl:.* (exp (* -0.039 (tte op)))
                           (magicl:.max 0 (magicl:.- final-value (strike op)))))))
            ((equal 'put (callput op))
             (colmean (magicl:.* (exp (* -0.039 (tte op)))
                                 (magicl:.max 0 (magicl:.- (strike op) final-value)))))))))
