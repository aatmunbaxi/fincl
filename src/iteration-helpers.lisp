;;;; iteration-helpers.lisp --- ITERATE drivers and clauses fincl uses
;;;;
;;;; Iteration patterns that recur across the library, each written once:
;;;;
;;;;   (for (key value) in-plist plist)           a property list, by pairs
;;;;   (for (t0 t1) in-intervals times)           consecutive grid times
;;;;   (for d in-dates start to end)              dates, END included
;;;;   (for d in-dates start below end)           dates, END excluded
;;;;   (for d in-business-days calendar from start to end)
;;;;   (collect-double x into v length n)         a typed double vector
;;;;
;;;; and SORTED-KEYS, a function over ITERATE's IN-HASHTABLE. The macros
;;;; expand at their use sites, so the date functions they call may be
;;;; defined later. Not exported.

(in-package #:fincl)

(defmacro-driver (for pair in-plist plist)
  "(for (key value) in-plist PLIST): each key of PLIST and its value."
  (let ((keyword (if generate 'generate 'for)))
    `(,keyword ,pair on ,plist by #'cddr)))

(defmacro-driver (for pair in-intervals times)
  "(for (t0 t1) in-intervals TIMES): TIMES[j] and TIMES[j+1] for each
interval j of the grid TIMES, a vector."
  (destructuring-bind (t0 t1) pair
    (let ((grid (gensym "GRID")) (j (gensym "J"))
          (keyword (if generate 'generate 'for)))
      `(progn (with ,grid = ,times)
              (,keyword ,j from 1 below (length ,grid))
              (for ,t0 = (aref ,grid (1- ,j)))
              (for ,t1 = (aref ,grid ,j))))))

(defmacro-driver (for d in-dates start &optional to end below limit)
  "(for d in-dates START to END): each date from START to END, both included;
(for d in-dates START below END): END excluded."
  (unless (or end limit)
    (signal-invalid-argument 'in-dates `(for ,d in-dates ,start) 'list
                             "an end: (for d in-dates start to end) or (... below end)"))
  (let ((last (gensym "END")) (next (gensym "NEXT"))
        (keyword (if generate 'generate 'for)))
    `(progn (with ,last = ,(or end limit))
            (with ,next = ,start)
            (,keyword ,d next (if ,(if end `(date< ,last ,next) `(not (date< ,next ,last)))
                                  (terminate)
                                  (prog1 ,next (setf ,next (date+ ,next 1))))))))

(defmacro-driver (for d in-business-days calendar from start to end)
  "(for d in-business-days CALENDAR from START to END): each business day of
CALENDAR from START to END, both included."
  (let ((cal (gensym "CALENDAR")) (last (gensym "END")) (next (gensym "NEXT"))
        (keyword (if generate 'generate 'for)))
    `(progn (with ,cal = ,calendar)
            (with ,last = ,end)
            (with ,next = (adjust ,cal ,start :following))
            (,keyword ,d next (if (date< ,last ,next)
                                  (terminate)
                                  (prog1 ,next
                                    (setf ,next (adjust ,cal (date+ ,next 1) :following))))))))

(defmacro-clause (collect-double expr into var length n)
  "Store EXPR's successive values in VAR, a (SIMPLE-ARRAY DOUBLE-FLOAT (N))
allocated once. The ITER form does not return VAR by itself: name it in a
FINALLY."
  (let ((i (gensym "I")))
    `(progn (with ,var = (make-array ,n :element-type 'double-float))
            (with ,i = 0)
            (setf (aref ,var ,i) ,expr)
            (incf ,i))))

(defun sorted-keys (table)
  "The keys of the hash table TABLE, sorted by name."
  (sort (iter (for (key nil) in-hashtable table) (collect key)) #'string<))
