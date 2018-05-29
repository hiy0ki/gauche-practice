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
      #?=(cons (car #?=a) (append2 (cdr a) #?=b))
      #?=b))

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


;; local

((lambda (a b) (+ (* a a ) (* b b))) 3 4)
;; lambdaの仮引数がローカル変数の正体そのもの
;; なので上と下の式は同じ意味になる
(let ((a 3)
      (b 4))
  (+ (* a a) (* b b)))


;; 可変長引数
(define (func a b . c)
  (print "a=" a ". b=" b ". c=" c))


;;; 0個以上の可変長引数 その1
(define list2
  (lambda a
    (if (null? a) '()
        (cons (car a) (apply list2 (cdr a))))))

;; 0個以上の可変長引数 その2
;; こっちのほうがわかりやすいきがする
(define (list3 . a)
  (if (null? a) '()
      (cons (car a) (apply list3 (cdr a)))))

;; pattern match
(define (append3 . args)
  (cond ((null? #?=args) '())
        ((null? (cdr args)) (car args))
        (else (append2 #?=(car args) (apply append3 (cdr args))))))


(use util.match)

(define (append4 . args)
  (match args
         (() '())
         ((a) a)
         ((a . b) (append2 a (apply append4 b)))))


;; make-list
(define (make-list2 num . args)
  (define (maker n init)
    (if (= n 0)
        '()
        (cons init (maker (- n 1) init))))
  (maker num (if (null? args) #f (car args))))

(define (make-list3 num . args)
  (let-optionals* args ((init #f))
                  (define (maker n)
                    (if (= n 0) '() (cons init (maker (- n 1)))))
                  (maker num)))

;; keyword引数
(define (parson . args)
  (let-keywords args ((name "Anonymous")
                      (age "unknown")
                      . other-args)
                (print name " is " age " year(s) old.")
                (print "other info: " other-args)))

;; 多値
(min&max 52 24 4302 135 4)

(use srfi-1)
(split-at '(1 2 3 4 5 6 7 8 9 10) 3)

;; 受け取る
(receive (min-val max-val)
         (min&max 4 2 1)
         (list min-val max-val))

(receive (min-val . rest)
         (min&max 4 2 1)
         (list min-val rest))

(receive all-values
         (min&max 4 2 1)
         all-values)

;; 返す
(values 1 2 3 4)
(values '(3 4 ) (* 45 2))


;; 真偽値
(equal? '(1 2 3) '(1 2 3))

(eq? '(1 2 3) '(1 2 3))

(define p '(1 2 3))

(eq? p p)

;; 6章 状態の管理
(define *inventory* (list 'potion 'potion 'dagger 'cookie 'dagger))

(member 'cookie *inventory*)

(define (member2 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (cond ((null? lis) #f)
                        ((cmp-fn elt (car lis)) lis)
                        (else (member elt (cdr lis) cmp-fn)))))

(define (member3 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) #f)
                          ((cmp-fn elt (car lis)) lis)
                          (else (loop (cdr lis)))))
                  (loop lis)))

(define *long-list* (make-list 1000000 #f))

(define (has-item? item)
  (member item *inventory*))

;; 条件に一致する要素を一つ削除する
(define (delete-1 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) '())
                          ((cmp-fn elt (car lis)) (cdr lis))
                          (else (cons (car lis) (loop (cdr lis))))))
                  (loop lis)))

(delete-1 'aaa '(a aaa bb aaa bbb cc ccc cc))


;; 要素が見つからなかった場合に一切コピーしないdelete-1を実装する
(define (delete-1-nc elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) '())
                          ((cmp-fn elt (car lis)) (cdr lis))
                          (else (cons (car lis) (loop (cdr lis))))))
                  (loop lis)))

(use gauche.test)
(let ((data (list 1 2 3 4 5)))
  (test* "non copy delete-1" data (delete-1-nc 6 data) eq?))


(define (delete-item! item)
  (set! *inventory* (delete-1 item *inventory*)))

(define (add-item! item)
  (set! *inventory* (add-item! item *inventory*)))

(define (add-item!2 item)
  (push! *inventory* item))

;; !がつくのは破壊的変更をする手続き。schemeの慣習

(define (assoc2 key alist . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop alis)
                    (cond ((null? alis) #f)
                          ((cmp-fn key (caar alis)) (car alis))
                          (else (loop (cdr alis)))))
                  (loop alist)))



