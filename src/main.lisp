(ql:quickload :magicl)
(defun constant-vol (s0 tte n-paths n-steps &key (sigma 0.2))
  (let* ((noise (magicl:random-normal `(,n-paths ,n-steps)))
         (dt (/ tte n-steps))
         (incrs (magicl:.+
                 (* dt
                    (+ 0.15  0.039 (* 0.5 (expt sigma 2))))
                 (magicl:.* sigma (magicl:.* (sqrt dt) noise))))
         (log-returns (cumsum incrs))
         (S (magicl:const s0 `(,n-paths ,n-steps) :type 'double-float)))
    (magicl:hstack `(,(magicl:slice S '(0 0) `(,n-paths 1))
                     ,(magicl:.* (magicl:.exp log-returns) S)))))
(lisp-stat:matrix-df
 (loop :for i :from 0 :to (- (cadr (magicl:shape *X*)) 1)
       :collect (make-symbol (format nil "~a" i)))
 (magicl:lisp-array *x*))

(plot:plot
 (vega:defplot simulations
   `(:title "Simulated paths"
     :data (:values ,x)
     :mark :lines)))

(defun cumsum (arr &key (axis 1))
  (let ((a (magicl:column  arr 0))
        (n (cadr (magicl:shape arr))))
    (loop :with v := `(,a)
          :for i :from 1 :upto (- n 1)
          :do (push (magicl:.+ (magicl:column arr i) (car v)) v)
          :finally (return (magicl:hstack (reverse v))))))
