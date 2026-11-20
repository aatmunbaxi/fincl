;;;; construction-equality.lisp --- CONSTRUCTION-EQUAL-P, the comparison of built objects
;;;;
;;;; Two objects are compared as data. Numbers must match exactly, including
;;;; their type (1 and 1d0 differ); strings are compared with case; dates by
;;;; value; standard objects and structures by class and every bound slot;
;;;; hash tables by keys and values; lists and vectors element by element. A
;;;; plain closure is not data: it compares equal to any closure, so the data
;;;; it was made from must sit in slots beside it (interpolated curves keep
;;;; their knots, and an interpolator is a standard object holding them).
;;;; EQUALP is not used: it never matches two closures, equates 1 and 1d0, and
;;;; ignores string case.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun construction-difference (a b &optional path)
  "NIL if A and B are equal as data, otherwise (path a b) for the first
difference found: PATH lists the slot names, indexes and keys leading to it,
outermost first, and A and B are the differing values.

  (construction-difference '(1 2) '(1 2d0)) => ((1) 2 2.0d0)"
  (flet ((differ () `(,(reverse path) ,a ,b))
         (sub (x y key) (construction-difference x y (cons key path))))
    (cond
      ((eq a b) nil)
      ((or (numberp a) (numberp b))
       (unless (and (numberp a) (numberp b) (eql a b)) (differ)))
      ((or (stringp a) (stringp b))
       (unless (and (stringp a) (stringp b) (string= a b)) (differ)))
      ((or (datep a) (datep b))
       (unless (and (datep a) (datep b) (date= a b)) (differ)))
      ((or (hash-table-p a) (hash-table-p b))
       (cond ((not (and (hash-table-p a) (hash-table-p b)
                        (eq (hash-table-test a) (hash-table-test b))
                        (= (hash-table-count a) (hash-table-count b))))
              (differ))
             (t (iter (for (key value) in-hashtable a)
                      (multiple-value-bind (other found) (gethash key b)
                        (if found
                            (a:when-let ((d (sub value other key))) (return d))
                            (return (sub value :missing key))))))))
      ((or (typep a '(or standard-object structure-object))
           (typep b '(or standard-object structure-object)))
       (if (and (eq (class-of a) (class-of b))
                (typep a '(or standard-object structure-object)))
           (iter (for slot in (c2mop:class-slots (class-of a)))
                 (for name = (c2mop:slot-definition-name slot))
                 (for bound = (slot-boundp a name))
                 (cond ((not (eq bound (slot-boundp b name))) (return (differ)))
                       (bound (a:when-let ((d (if (and (eq name 'fincl::underlyings)
                                                        (typep a 'fincl::composite-market))
                                                   ;; The underlyings are a set: their order is unspecified.
                                                   (sub (sort (copy-list (slot-value a name)) #'string<)
                                                        (sort (copy-list (slot-value b name)) #'string<)
                                                        name)
                                                   (sub (slot-value a name) (slot-value b name) name))))
                                (return d)))))
           (differ)))
      ((or (functionp a) (functionp b))
       (unless (and (functionp a) (functionp b)) (differ)))
      ((or (consp a) (consp b))
       (if (not (and (consp a) (consp b)))
           (differ)
           (iter (for ta initially a then (cdr ta))
             (for tb initially b then (cdr tb))
             (for i from 0)
             (if (and (consp ta) (consp tb))
                 (a:when-let ((d (sub (car ta) (car tb) i))) (return d))
                 (return (sub ta tb :tail))))))
      ((or (arrayp a) (arrayp b))
       (cond ((not (and (arrayp a) (arrayp b)
                        (equal (array-dimensions a) (array-dimensions b))
                        (equal (array-element-type a) (array-element-type b))))
              (differ))
             (t (iter (for i below (array-total-size a))
                      (a:when-let ((d (sub (row-major-aref a i) (row-major-aref b i) i)))
                        (return d))))))
      (t (unless (eql a b) (differ))))))

(defun construction-equal-p (a b)
  "True if A and B are equal as data (CONSTRUCTION-DIFFERENCE finds none)."
  (null (construction-difference a b)))

(define-checks construction-equality-checks
  (format t "~&CONSTRUCTION-EQUAL-P~%")
  (let ((curve (lambda (rate)
                 (make-interpolated-zero-curve
                                :dates (list #D"2026-09-24" #D"2027-09-24" #D"2029-09-24")
                                :zero-rates (list 0.03d0 rate 0.04d0)))))
    (check-that "the same numbers, lists, dates and strings are equal"
                (construction-equal-p `(1 2.5d0 "ab" ,#D"2027-09-24" (:x (3)))
                                      `(1 2.5d0 "ab" ,(make-date 2027 9 24) (:x (3)))))
    (check-that "a changed number is a difference, and the report gives its place"
                (equal (construction-difference '(1 (2 3d0)) '(1 (2 3.0000001d0)))
                       '((1 1) 3d0 3.0000001d0)))
    (check-that "a changed number type is a difference: 1 against 1d0"
                (not (construction-equal-p '(:spot 1) '(:spot 1d0))))
    (check-that "a string differing in case is a difference"
                (not (construction-equal-p "Usd" "usd")))
    (check-that "a different date is a difference"
                (not (construction-equal-p #D"2027-09-24" #D"2027-09-25")))
    (check-that "lists of different length differ"
                (and (not (construction-equal-p '(1 2) '(1 2 3)))
                     (not (construction-equal-p '(1 2 3) '(1 2)))))
    (check-that "vectors differ by element type and by element"
                (and (not (construction-equal-p (vector 1d0 2d0)
                                                (make-array 2 :element-type 'double-float
                                                              :initial-contents '(1d0 2d0))))
                     (not (construction-equal-p (vector 1d0 2d0) (vector 1d0 3d0)))))
    (check-that "interpolated curves with the same knots are equal, with a changed knot not"
                (and (construction-equal-p (funcall curve 0.035d0) (funcall curve 0.035d0))
                     (not (construction-equal-p (funcall curve 0.035d0) (funcall curve 0.036d0)))))
    (check-that "the report names the slot of a changed knot"
                (member 'fincl::knot-values
                        (first (construction-difference (funcall curve 0.035d0) (funcall curve 0.036d0)))))
    (check-that "objects of different classes differ"
                (not (construction-equal-p (make-instance 'fincl::flat-yield-curve :rate 0.03d0)
                                           (make-instance 'fincl::flat-dividend-curve :rate 0.03d0))))
    (check-that "two closures are equal, a closure and a number are not"
                (and (construction-equal-p (lambda (x) x) (lambda (x) (1+ x)))
                     (not (construction-equal-p (lambda (x) x) 1))))
    (let ((a (make-hash-table :test 'equal)) (b (make-hash-table :test 'equal)))
      (setf (gethash :x a) 1 (gethash :x b) 1)
      (check-that "hash tables compare by key and value"
                  (and (construction-equal-p a b)
                       (progn (setf (gethash :x b) 2) (not (construction-equal-p a b)))
                       (progn (setf (gethash :x b) 1 (gethash :y b) 1)
                              (not (construction-equal-p a b))))))
    (check-that "markets built twice from the same specs are equal; a changed spot is not"
                (flet ((market (spot)
                         (make-market :valuation-date *today*
                                      :underlyings `((:x :spot ,spot :vol 0.2d0))
                                      :curves '((:c :rate 0.03d0)))))
                  (and (construction-equal-p (market 100) (market 100))
                       (not (construction-equal-p (market 100) (market 101)))
                       (member 'fincl::spots
                               (first (construction-difference (market 100) (market 101)))))))))
