;;;; pricing.lisp --- pricing on composite markets
;;;;
;;;; Engines implement PRICE against the single-underlying market protocol. On a
;;;; COMPOSITE-MARKET, this :AROUND method narrows the market to a MARKET-VIEW for the
;;;; instrument (its underlying, which discounts on the curve of its currency). Then it
;;;; prices again on the view. Interest-rate instruments have no underlying. They price on
;;;; the composite market itself.
;;;;
;;;; This method must run before every other :AROUND method, in particular the :AROUND
;;;; method of the option (expiry checks, validation, restarts). That method reads the
;;;; market through the protocol that a view implements. The argument precedence order of
;;;; PRICE is (MARKET INSTRUMENT ENGINE). Thus a method that is specialized on
;;;; COMPOSITE-MARKET is more specific than any method whose market specializer is T, in
;;;; any instrument specializer. All primary methods specialize the market as T, so their
;;;; order does not change.

(in-package #:fincl)

(defgeneric instrument-view (market instrument)
  (:documentation "The market INSTRUMENT is priced on: for most instruments
the MARKET-VIEW of the composite MARKET for its underlying (or the market's
only one), discounting on the curve of the underlying's currency (an
instrument paying in another currency is a quanto and signals
UNSUPPORTED-QUANTO); for interest-rate instruments MARKET itself."))

(defmethod instrument-view (market (instrument rates-instrument))
  market)

(defmethod instrument-view (market (instrument instrument))
  (let* ((underlying (resolve-underlying market (instrument-underlying instrument)))
         (underlying-currency (gethash underlying (market-underlying-currency market)))
         (currency (instrument-currency instrument)))
    (when (and currency (not (eq currency underlying-currency)))
      (error 'unsupported-quanto :underlying underlying :underlying-currency underlying-currency
                                  :currency currency))
    (market-view market :underlying underlying)))

(defmethod price :around ((instrument instrument) engine (market composite-market))
  (let ((view (instrument-view market instrument)))
    (if (eq view market)
        (call-next-method)
        (price instrument engine view))))

(defun pricing-market (market instrument)
  "The market INSTRUMENT is priced on: its view of MARKET if MARKET is
composite, else MARKET itself."
  (if (typep market 'composite-market)
      (instrument-view market instrument)
      market))

(defun market-with-flat-vol (market underlying vol)
  "A copy of the composite MARKET with UNDERLYING's vol structure replaced by
a flat VOL."
  (market-with market :vol underlying (make-flat-black-vol :vol vol)))
