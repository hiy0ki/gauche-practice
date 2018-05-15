#!/usr/bin/env gosh

(define (main args)
  (print (cdr args))
  (print "hello world")
  (print (len2 (cdr args))))

(define (1+ x)
  (+ 1 x))

(define (len2 x)
  (cond ((null? x) 0)
        (else (1+ (len2 (cdr x))))))


