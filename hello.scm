#!/usr/bin/env gosh

(define (main args)
  (print (cdr args))
  (print "hello world")
  (print (len2 (cdr args)))
  0)

(define (1+ x)
  (+ 1 x))

(define (len2 x)
  (cond ((null? x) 0)
        (else (1+ (len2 (cdr x))))))

(define (pythagoras x y)
  (sqrt (+ (* x x) (* y y))))


(define (fold2 proc init lis)
  (if (null? lis)
      init
      (fold2 proc (car lis) init) (cdr lis))))


(define (last-pair2 lis)
  (if (pair? (cdr lis))
      (last-pair2 (cdr lis))
      lis))

(define (copy-list lis)
  (if (pair? lis)
      (cons (car lis) (copy-list (cdr lis)))
      lis))

(define (append2 a b)
  (if (pair? a)
      (cons (car a) (append2 (cdr a) b))
      b))

(define (reverse1 lis)
  (fold2 cons '() lis))

(define (reverse2 lis)
  (if (pair? lis)
      (append2 (reverse2 (cdr lis)) (list (car lis)))
      lis))


(define (find2 pred lis)
  (cond ((null? lis) #f)
        ((pred (car lis)) (car lis))
        (else (find2 pred (cdr lis)))))

;; 末尾再帰バージョンのlength
(define (length3 lis)
  (define (length-rec lis n)
    (if (null? lis)
        n
        (length-rec (cdr lis) (+ n 1))))
  (length-rec lis 0))
