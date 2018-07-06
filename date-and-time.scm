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

