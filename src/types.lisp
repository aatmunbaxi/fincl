;;;; types.lisp -- named types for the quantities fincl computes with
;;;;
;;;; Each type is an interval or a MEMBER type. Thus it costs nothing at run time, and it
;;;; documents the range of a slot or an argument. The types are subtypes of DOUBLE-FLOAT (or
;;;; of a vector of them). They are not distinct from one another beyond their ranges. A
;;;; volatility and a variance are both NON-NEGATIVE-DOUBLE, so the compiler cannot tell one
;;;; from the other. A slot type is a check on some implementations only. Validation that
;;;; must happen everywhere goes through CHECK-INPUT.

(in-package #:fincl)

(deftype non-negative-double ()
  "A DOUBLE-FLOAT at or above zero, e.g. 0d0 or 0.2d0."
  '(double-float 0d0))

(deftype positive-double ()
  "A DOUBLE-FLOAT above zero, e.g. 1d-12 or 100d0."
  '(double-float (0d0)))

(deftype unit-interval ()
  "A DOUBLE-FLOAT in [0, 1]: a probability, weight or cumulative probability,
e.g. 0.5d0."
  '(double-float 0d0 1d0))

(deftype correlation ()
  "A DOUBLE-FLOAT in [-1, 1], e.g. -0.7d0."
  '(double-float -1d0 1d0))

(deftype market-time ()
  "A horizon in years from the valuation date. A non-negative DOUBLE-FLOAT, for
example 0.25d0."
  'non-negative-double)

(deftype year-fraction ()
  "A day-count fraction of a year. It has a sign: a fraction from a later date to an
earlier date is negative, for example -0.5d0."
  'double-float)

(deftype volatility ()
  "A non-negative volatility per unit square-root time, e.g. 0.2d0. A variance
is a NON-NEGATIVE-DOUBLE."
  'non-negative-double)

(deftype rate ()
  "A continuously compounded rate per year. Signed, e.g. 0.04d0 or -0.005d0."
  'double-float)

(deftype discount-value ()
  "A discount or carry factor, a positive DOUBLE-FLOAT, e.g. 0.96d0."
  'positive-double)

(deftype complex-double ()
  "A complex number with DOUBLE-FLOAT parts, the value of a characteristic
function, e.g. #c(1d0 0d0)."
  '(complex double-float))

(deftype option-kind ()
  "The side of a call or put payoff: :CALL or :PUT."
  '(member :call :put))

(deftype double-vector ()
  "A one-dimensional simple array of DOUBLE-FLOAT."
  '(simple-array double-float (*)))

(deftype state-vector ()
  "The state of a process at one date: element 0 is the observable."
  'double-vector)

(deftype index ()
  "A non-negative array or grid index."
  '(integer 0 #.array-dimension-limit))
