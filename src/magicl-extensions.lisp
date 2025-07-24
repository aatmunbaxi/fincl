(in-package #:fincl)

(defun .mean  (x)
  (/ (magicl:sum x) (magicl:size x)))


;; (magicl:define-extensible-function (std-deviation std-deviation-lisp) (samples)
;;   (:method (samples)
;;     (let* ((sample-mean (/ (magicl:sum samples) (car (magicl:shape samples))))
;;            (n (car (magicl:shape samples))))
;;       (sqrt (/ (magicl:sum (magicl:.^ (magicl:.- samples sample-mean) 2)) n)))))


(defun cumsum (arr)
  (let ((a (magicl:column  arr 0))
        (n (cadr (magicl:shape arr))))
    (loop :with v := `(,a)
          :for i  fixnum :from 1 :below n
          :do (push (magicl:.+ (magicl:column arr i) (car v)) v)
          :finally (return (magicl:hstack (reverse v))))))
