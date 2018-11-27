#!/usr/local/bin/gosh

(use util.list)
(use srfi-1)
(use srfi-19)
(use www.cgi)
(use text.html-lite)
(use gauche.collection)

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

(define (days-of-month date)
  (- (date->modified-julian-day (next-month date))
     (date->modified-julian-day (first-day-of-month date))))

(define (date-slices-of-month date)
  (slices (append (make-list (date-week-day (first-day-of-month date)) #f)
                  (iota (days-of-month date) 1))
          7 #t #f))

(define (month->link date content)
  (html:a :href #`"?y=,(date-year date)&=,(date-month date)" content))

(define (date-cell year month date)
  (if date
      (html:a :href #`"?y=,|year|&m=,|month|&d=,|date|" date)
      ""))

(define (calendar date)
  (html:table
   (html:tr (html:td (month->link (prev-month date) "<-"))
            (html:td :colspan 5 :align "center"
                     #`",(date-year date)/,(date-month date)")
            (html:td (month->link (next-month date) "->")))
   (html:tr (map html:td "日月火水木金土"))
   (map (lambda (w)
          (html:tr
           (map (lambda (d)
                  (html:td (date-cell (date-year date) (date-month date) d)))
                w)))
        (date-slices-of-month date))))


(define (page . content)
  `(,(cgi-header)
    ,(html:html :lang "ja"
                (html:head
                       (html:meta :charset "utf-8")
                       (html:title "簡易スケジュール表"))
                      (apply html:body content))))

(define (cmd-show-calendar y m)
  (page
   (if (and y m (<= 1 m 12) (<= 1753 y))
       (calendar (make-month m y))
       (calendar (current-date)))))

(define (cmd-show-plan y m d)
  (page
   (calendar (make-month m y))
   (html:form
    (html:p #`",|y|年,|m|月,|d|日の予定")
    (html:input :type "hidden" :name "c" :value "c")
    (html:input :type "hidden" :name "y" :value (x->string y))
    (html:input :type "hidden" :name "m" :value (x->string m))
    (html:input :type "hidden" :name "d" :value (x->string d))
    (html:p (html:textarea :rows 8 :cols 40 :name "p"
                           (html-escape-string "予定を記入")))
    (html:p (html:input :type "submit" :name "submit" :value "変更")))))


(define (main args)
  (cgi-main
   (lambda (params)
     (let ((y (cgi-get-parameter "y" params :convert x->integer))
           (m (cgi-get-parameter "m" params :convert x->integer))
           (d (cgi-get-parameter "d" params :convert x->integer)))
       (if (and y m d)
           (cmd-show-plan y m d)
           (cmd-show-calendar y m))
       ))))




