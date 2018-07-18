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


;; ユリウス日と改定ユリウス日
(use srfi-19)
(date->julian-day today)
(date->modified-julian-day today)

;; 日数計算
(define date1 (make-date 0 0 0 0 31 8 2007 (date-zone-offset (current-date))))
(define date2 (make-date 0 0 0 0 31 12 2006 (date-zone-offset (current-date))))

(- (date->modified-julian-day date1)
   (date->modified-julian-day date2))


(define (days-of-month date)
  (- (date->modified-julian-day (next-month date))
     (date->modified-julian-day (first-day-of-month date))))


(make-list
 (date-week-day
  (make-date 0 0 0 0 1 1 2007 (date-zone-offset (current-date))))
 #f)

(iota (days-of-month (current-date)) 1)

(slices
 (append
  (make-list
   (date-week-day
    (first-day-of-month (current-date))))
  (iota (days-of-month (current-date)) 1))
 7 #t #f)



