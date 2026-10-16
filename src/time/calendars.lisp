;;;; time/calendars.lisp --- country and exchange calendars
;;;;
;;;; Ported from QuantLib 1.43 (ql/time/calendars/target.cpp, unitedstates.cpp,
;;;; unitedkingdom.cpp). tests/fixtures/calendars.sexp, generated from
;;;; QuantLib, lists every holiday of each calendar from 1990 to 2060 and is
;;;; the reference; rules before 1990 follow QuantLib's code but are not
;;;; checked against it.

(in-package #:fincl)

;;; Christmas or Boxing Day on a weekend is replaced two days later: Christmas
;;; on Saturday -> Monday 27th, on Sunday -> Tuesday 27th (Boxing Day takes
;;; the Monday); Boxing Day on Saturday -> Monday 28th, on Sunday -> Tuesday
;;; 28th.
(define-observance :christmas-substitute (date)
  (if (>= (day-of-week date) 6) (date+ date 2) date))

;;; --------------------------------------------------------------------
;;; Euro area
;;; --------------------------------------------------------------------

(define-calendar target (weekends-only)
  "TARGET, the euro area's payment system."
  (:fixed 1 1)
  (:easter -2 :from 2000)                      ; Good Friday
  (:easter 1 :from 2000)                       ; Easter Monday
  (:fixed 5 1 :from 2000)                      ; Labour Day
  (:fixed 12 25)
  (:fixed 12 26 :from 2000)                    ; Day of Goodwill
  (:dates "1998-12-31" "1999-12-31" "2001-12-31"))

;;; --------------------------------------------------------------------
;;; United States
;;; --------------------------------------------------------------------

(define-calendar united-states-settlement (weekends-only)
  "United States settlement days: federal holidays, moved to the nearest
weekday."
  (:fixed 1 1 :observe :nearest-weekday)       ; New Year's Day
  (:nth-weekday 3 :monday 1 :from 1983)        ; Martin Luther King Jr.
  (:nth-weekday 3 :monday 2 :from 1971)        ; Washington's Birthday
  (:fixed 2 22 :observe :nearest-weekday :to 1970)
  (:nth-weekday -1 :monday 5 :from 1971)       ; Memorial Day
  (:fixed 5 30 :observe :nearest-weekday :to 1970)
  (:fixed 6 19 :observe :nearest-weekday :from 2022) ; Juneteenth
  (:fixed 7 4 :observe :nearest-weekday)       ; Independence Day
  (:nth-weekday 1 :monday 9)                   ; Labor Day
  (:nth-weekday 2 :monday 10 :from 1971)       ; Columbus Day
  (:fixed 11 11 :observe :nearest-weekday :to 1970) ; Veterans Day
  (:nth-weekday 4 :monday 10 :from 1971 :to 1977)
  (:fixed 11 11 :observe :nearest-weekday :from 1978)
  (:nth-weekday 4 :thursday 11)                ; Thanksgiving
  (:fixed 12 25 :observe :nearest-weekday))    ; Christmas

(define-calendar nyse (weekends-only)
  "New York Stock Exchange."
  (:fixed 1 1 :observe :sunday-to-monday)      ; New Year's Day (not moved to Friday)
  (:nth-weekday 3 :monday 1 :from 1998)        ; Martin Luther King Jr.
  (:nth-weekday 3 :monday 2 :from 1971)        ; Washington's Birthday
  (:fixed 2 22 :observe :nearest-weekday :to 1970)
  (:easter -2)                                 ; Good Friday
  (:nth-weekday -1 :monday 5 :from 1971)       ; Memorial Day
  (:fixed 5 30 :observe :nearest-weekday :to 1970)
  (:fixed 6 19 :observe :nearest-weekday :from 2022) ; Juneteenth
  (:fixed 7 4 :observe :nearest-weekday)       ; Independence Day
  (:nth-weekday 1 :monday 9)                   ; Labor Day
  (:nth-weekday 4 :thursday 11)                ; Thanksgiving
  (:fixed 12 25 :observe :nearest-weekday)     ; Christmas
  ;; Presidential election days: every year to 1968, then every fourth.
  (:nth-weekday 1 :tuesday 11 :to 1980 :except (1969 1970 1971 1973 1974 1975 1977 1978 1979))
  ;; Closed on Wednesdays from June 12th, 1968 (the paperwork crisis).
  (:when (lambda (y m d)
           (and (>= (+ (- (date-serial (make-date y m d)) (date-serial (make-date y 1 1))) 1) 163)
                (= (day-of-week (make-date y m d)) 3)))
   :from 1968 :to 1968)
  (:dates "2025-01-09"                         ; President Carter's funeral
          "2018-12-05"                         ; President Bush's funeral
          "2012-10-29" "2012-10-30"            ; Hurricane Sandy
          "2007-01-02"                         ; President Ford's funeral
          "2004-06-11"                         ; President Reagan's funeral
          "2001-09-11" "2001-09-12" "2001-09-13" "2001-09-14"
          "1994-04-27"                         ; President Nixon's funeral
          "1985-09-27"                         ; Hurricane Gloria
          "1977-07-14"                         ; Blackout
          "1973-01-25" "1972-12-28"            ; Presidents Johnson's and Truman's funerals
          "1969-07-21" "1969-03-31" "1969-02-10"
          "1968-07-05" "1968-04-09" "1963-11-25" "1961-05-29" "1958-12-26"
          "1954-12-24" "1956-12-24" "1965-12-24"))

(define-calendar united-states-government-bond (weekends-only)
  "United States government bond market (SIFMA recommendations)."
  (:fixed 1 1 :observe :sunday-to-monday)      ; New Year's Day
  (:nth-weekday 3 :monday 1 :from 1983)        ; Martin Luther King Jr.
  (:nth-weekday 3 :monday 2 :from 1971)        ; Washington's Birthday
  (:fixed 2 22 :observe :nearest-weekday :to 1970)
  ;; Good Friday; since 1996 only an early close when it is the first
  ;; Friday of April (the payrolls release).
  (:easter -2 :to 1995)
  (:when (lambda (y m d)
           (and (date= (make-date y m d) (date+ (easter y) -2)) (> d 7)))
   :from 1996)
  (:nth-weekday -1 :monday 5 :from 1971)       ; Memorial Day
  (:fixed 5 30 :observe :nearest-weekday :to 1970)
  (:fixed 6 19 :observe :nearest-weekday :from 2022) ; Juneteenth
  (:fixed 7 4 :observe :nearest-weekday)       ; Independence Day
  (:nth-weekday 1 :monday 9)                   ; Labor Day
  (:nth-weekday 2 :monday 10 :from 1971)       ; Columbus Day
  (:fixed 11 11 :observe :sunday-to-monday :to 1970) ; Veterans Day, no Friday
  (:nth-weekday 4 :monday 10 :from 1971 :to 1977)
  (:fixed 11 11 :observe :sunday-to-monday :from 1978)
  (:nth-weekday 4 :thursday 11)                ; Thanksgiving
  (:fixed 12 25 :observe :nearest-weekday)     ; Christmas
  (:dates "2018-12-05" "2012-10-30" "2004-06-11"))

;;; --------------------------------------------------------------------
;;; United Kingdom
;;; --------------------------------------------------------------------

(define-calendar united-kingdom-settlement (weekends-only)
  "United Kingdom settlement days: England and Wales bank holidays."
  (:fixed 1 1 :observe :next-monday)           ; New Year's Day
  (:easter -2)                                 ; Good Friday
  (:easter 1)                                  ; Easter Monday
  (:nth-weekday 1 :monday 5 :except (1995 2020)) ; Early May (V.E. day years: May 8th)
  (:nth-weekday -1 :monday 5 :except (2002 2012 2022)) ; Spring (jubilees: June)
  (:nth-weekday -1 :monday 8)                  ; Summer
  (:fixed 12 25 :observe :christmas-substitute) ; Christmas
  (:fixed 12 26 :observe :christmas-substitute) ; Boxing Day
  (:dates "1995-05-08" "2020-05-08"            ; V.E. day anniversaries
          "2002-06-03" "2002-06-04"            ; Golden Jubilee
          "2012-06-04" "2012-06-05"            ; Diamond Jubilee
          "2022-06-02" "2022-06-03"            ; Platinum Jubilee
          "2011-04-29"                         ; Royal Wedding
          "2022-09-19"                         ; The Queen's funeral
          "2023-05-08"                         ; Coronation
          "1999-12-31"))

(define-calendar united-kingdom-exchange (united-kingdom-settlement)
  "London Stock Exchange: the same holidays as UK settlement.")

(define-calendar united-states-sofr (united-states-government-bond)
  "SOFR fixings: the government bond market's days, closed on every Good
Friday (SOFR has never fixed on one)."
  (:easter -2))
