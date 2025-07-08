(defclass american-option (option)
  ())

(defmethod mc-pricer ((op american-option) spot sims steps proc)
  "Compute American option price with the Longstaff-Schwarz method"
  (let* ((paths (gen-paths proc spot (tte op) sims steps))
         (dt (/ (tte op) steps))
         (discount (exp (* -0.039  dt)))
         ())))
