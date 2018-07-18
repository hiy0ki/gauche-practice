(use srfi-19)

(define today (current-date))

(date-nanosecond today)
(date-second today)
(date-minute today)
(date-hour today)
(date-day today)
(date-month today)
(date-year today)
(date-zone-offset today)

(make-date 0 0 0 0 1 1 2018 (date-zone-offset (current-date)))

(define (make-month m y)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (first-day-of-month date)
  (make-month (date-month date) (date-year date)))

(define (next-month date)
  (if (= (date-month date) 12)
      (make-month 1 (+ (date-year date) 1))
      (make-month (+ (date-month date) 1) (date-year date))))

(define (prev-month date)
  (if (= (date-month date) 1)
      (make-month 12 (- (date-year date) 1))
      (make-month (- (date-month date) 1) (date-year date))))


(use srfi-19)
(date->julian-day today)

