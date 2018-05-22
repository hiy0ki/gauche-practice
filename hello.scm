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


(define (tree-walk walker proc tree)
  (walker (lambda (elt)
            (if (list? elt)
                (tree-walk walker proc elt)
                (proc elt)))
          tree))

(define (reverse-for-each proc lis)
  (for-each proc (reverse lis)))

(define (reverse-map proc lis)
  (map proc (reverse lis)))

(define (reversed walker)
  (lambda (proc lis)
    (walker proc (reverse lis))))



(define (filter2 proc lis)
  (cond ((null? lis) '())
        ((proc (car lis)) (cons (car lis) (filter2 proc (cdr lis))))))

(define (fil2 proc lis)
  (cond ((null? lis) lis)
        ((proc (car lis))
         (cons (car lis) (fil2 proc (cdr lis))))
        (else (fil2 proc (cdr lis)))))


;; p68 課題
(define (for-each-numbers proc lis)
  (for-each proc (fil2 number? lis)))

(define (map-numbers proc lis)
  (map proc (fil2 number? lis)))

(define (numbers-only walker)
  (lambda (proc lis)
    (walker proc (fil2 number? lis))))



      
            

















                       
