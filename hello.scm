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
      (fold2 proc (car lis) (cdr lis))))


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

(define (has-item? player item)
  (member item (cdr (assoc 'inventory player))))

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



;; 似たようなパターンをまとめる
(define (traverse fallback get-key return repeat)
  (lambda (elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
                    (define (loop lis)
                      (cond ((null? lis) fallback)
                            ((cmp-fn elt (get-key lis)) (return lis))
                            (else (repeat loop lis))))
                    (loop lis))))

(define member4
  (traverse #f car values
            (lambda (loop lis) (loop (cdr lis)))))

(define delete-1
  (traverse '() car cdr
            (lambda (loop lis) (cons (car lis) (loop (cdr lis))))))

(define assoc3
  (traverse #f caar car
            (lambda (loop lis) (loop (cdr lis)))))


;; game
(define *item-database*
  '((potion (drink . #t) (throw . #t))
    (elixir (drink . #t) (throw . #t))
    (pancake (eat . #t) (throw . #t))
    (cookie (eat . #t) (throw . #t))
    (dagger (throw . #t))))

(define (item-properties item)
  (cond ((assoc item *item-database*) => cdr)
        (else '())))

(define (item-property-get item prop)
  (cond ((assoc prop (item-properties item)) => cdr)
        (else #f)))

(define (use-item! what item)
  (cond ((not (has-item? item))
         (print item "を持っていません"))
        ((not (item-property-get item what))
         (print item "を" what "することはできません"))
        (else
         (print what " " item)
         (delete-item! item)))
  #t)

(define (make-player . args)
  (define (loop lis)
    (match lis
           (() '())
           ((attr value . rest) (cons (cons attr value) (loop rest)))))
  (loop args))

;; named-let
(define (make-player2 . args)
  (let loop ((lis args))
    (match lis
           (() '())
           ((attr value . rest) (cons (cons attr value) (loop rest))))))

(define *player*
  (make-player2 'hp 300 'mp 66 'position #f 'inventory '(potion potion dagger cookie dagger)))

;; set!
(define p (cons 1 2))
(set! (car p) 3)
(set! (cdr p) 4)
(set! (cdr p) (cons 5 (cdr p)))
(push! (cdr p) 10)
(push! p 20)


(define (add-item! player item)
  (let ((p (assoc 'inventory player)))
    (push! (cdr p) item)))

(define (delete-item! player item)
  (let ((p (assoc 'inventory player)))
    (set! (cdr p) (delete-1 item (cdr p)))))

(define (get-hp player)
  (cdr (assoc 'hp player)))

(define (add-hp! player n)
  (let ((p (assoc 'hp player)))
    (set! (cdr p) (+ n (cdr p)))))

(define (get-player-attr player attr)
  (cdr (assoc attr player)))

(define (update-player-attr! player attr updater)
  (let ((p (assoc attr player)))
    (set! (cdr p) (updater (cdr p)))))

(define (get-inventory player)
  (get-player-attr player 'inventory))

(define (has-item? player item)
  (member item (get-inventory player)))

(define (delete-item! player item)
  (update-player-attr! player 'inventory (cut delete-1 item <>)))

(define (add-item! item)
  (update-player-attr! player 'inventory (cut cons item <>)))

;; 準クオート quasiquote
`(* 0 (+ 1 2))
`(* 3 ,(+ 1 2))



;; hash-table 順序は不定
(define table (make-hash-table))

(hash-table-put! table 'name "Gauche")
(hash-table-put! table 'version "0.9")

(hash-table-get table 'name)
(hash-table-get table 'version)
(hash-table-get table 'encodeing) ; error

;; string をkeyにする
(hash-table-put! table "key1" 1)

(hash-table-get table "key1") ; error because use eq? function

(define table2 (make-hash-table 'string=?))

(hash-table-put! table2 "key1" 1)
(hash-table-get table2 "key1") ; get!

;; 一つのkeyに複数登録
(hash-table-push! table 'key "aaa")
(hash-table-push! table 'key "bbb")
(hash-table-get table 'key) ; ("bbb" "aaa")

(hash-table-pop! table 'key) ; "bbb"
(hash-table-get table 'key) ; ("aaa")

;; get all keys
(hash-table-keys table)
;; get all values
(hash-table-values table)

;; まとめて作成
(define table3 (hash-table 'eq? '(key1 . 1) '(key2 . 2) '(key3 . 3)))
(hash-table-keys table3)
(hash-table-values table3)

(hash-table-map table3 (lambda (key value) (* 5 value))) 

;; ストリーム、遅延リスト
(use util.stream)

(define fib (stream-cons 0 (stream-cons 1 (stream-map + fib (stream-cdr fib)))))

(stream->list (stream-take fib 10))
(stream-ref fib 10000) ; 任意の位置にある要素の値を取り出す


;; generic function
(ref '(a b c d e) 2) ; c
(ref '#(a b c d e) 2) ; c
(ref "abcde" 2) ; c

;; collection: 値のあつまり
(use gauche.collection)
(coerce-to <list> "abc")
(group-collection '(1 2 3  1  2 3  1 1 1 1 2 2  3 3 3 2 1))

(group-collection '(1 2 3  1  2 3  1 1 1 1 2 2  3 3 3 2 1) :key odd?)


;; sequence: collection + 要素の順序
(use gauche.sequence)
(subseq "abcde" 2 5)

(class-precedence-list <string>)


;; class
(define-class <book> ()
  ((author :init-keyword :author :init-value "詠み人知らず")
   (title :init-keyword :title :init-value "no title")
   (timestamp :init-form (sys-time))))

;; make
(d (make <book> :outhor "John" :title "Hogehoge"))

(define gauche-book
  (make <book> :author "へそ曲がり算法騎士団" :title "programing Gauche"))

(ref gauche-book 'author)
(ref gauche-book 'title)
(ref gauche-book 'timestamp)

(set! (ref gauche-book 'author) "Kahua project")

(ref gauche-book 'author)

(class-precedence-list <book>)



;; continuation
(define (find-fold pred? proc seed lis)
  (cond ((null? lis) seed)
        ((pred? (car lis))
         (let ((seed2 (proc (car lis) seed)))
           (find-fold pred? proc seed2 (cdr lis))))
        (else
         (find-fold pred? proc seed (cdr lis)))))

(define (process elt seed)
  (print "found: " elt)
  (cons elt seed))

(find-fold odd? process '() '(1 2 3 4 5 6 7 8 9 10))

;; let -> lambda
(define (find-fold pred? proc seed lis)
  (cond ((null? lis) seed)
        ((pred? (car lis))
         ((lambda (seed2)
            (find-fold pred? proc seed2 (cdr lis)))
          (proc (car lis) seed)))
        (else
         (find-fold pred? proc seed (cdr lis)))))

(find-fold odd? process '() '(1 2 3 4 5 6 7 8 9 10))

;; lambda -> continuation
(define (find-fold pred? proc/cont seed lis)
  (cond ((null? lis) seed)
        ((pred? (car lis))
         (proc/cont (car lis)
                    seed
                    (lambda (seed2)
                      (find-fold pred? proc/cont seed2 (cdr lis)))))
        (else
         (find-fold pred? proc/cont seed (cdr lis)))))

(define (process/cont elt seed cont)
  (print "found: " elt)
  (cont (cons elt seed)))

(find-fold odd? process/cont '() '(1 2 3 4 5 6 7 8 9 10))

(define next #f)
(define (break val) val)

(define (process/break elt seed cont)
  (set! next
        (lambda ()
          (print "found: " elt)
          (cont (cons elt seed))))
  (break #f))

(find-fold odd? process/break '() '(1 2 3 4 5 6 7 8 9 10))

(next)
(next)
(next)

(define (find-fold/cont pred? proc/cont seed lis cont)
  (cond [(null? lis) (cont seed)]
        [(pred? (car lis))
         (proc/cont (car lis)
                    seed
                    (lambda (seed2)
                      (find-fold/cont pred? proc/cont seed2 (cdr lis) cont)))]
        [else
         (find-fold/cont pred? proc/cont seed (cdr lis) cont)]))

(find-fold/cont odd? process/cont '() (iota 10 1) print)

;; call/cc
;; call with current continuation
(define (process/cc elt seed)
  (call/cc
   (lambda (cont)
     (print "found: " elt)
     (cont (cons elt seed)))))


(find-fold odd? process/cc '() (iota 10 1)) ; L:483's find-fold

(define (breaker/cc proc break)
  (lambda (elt seed)
    (call/cc
     (lambda (cont)
       (set! next (lambda () (cont (proc elt seed))))
       (break #f)))))

(call/cc
 (lambda (cont0)
   (find-fold odd? (breaker/cc process cont0)
              '() (iota 10 1))))

(next)

(apropos 'call-with)

;; 大域脱出
(define-syntax block
  (syntax-rules ()
    [(_ escape body ...)
     (call/cc (lambda (escape) body ...))]))

(block escape-top
       (block escape-1st
              (block escape-2nd
                     (escape-1st 1)
                     (print 'NG!!!)
                     )
              (print 'NG!!)
              )
       (print 'OK)
       )

(+ 1 2 (block return ; return を識別子に設定
              (print 'one)
              (print 'two)
              (return 3) ; ここで脱出
              (print 'four))
   4) ; (+ 1 2 3 4) が評価

;; break/next付きfor-each
(define-syntax for-each-ext
  (syntax-rules ()
    [(_ break next lambda-expr arg-list ...)
     (let ((arg1 (list arg-list ...)))
       (call/cc (lambda (break)
                  (apply for-each
                         (lambda arg
                           (call/cc (lambda (next)
                                      (apply lambda-expr arg))))
                         arg1))))]))

(for-each-ext break1 next1
              (lambda (x)
                (for-each-ext break2 next2
                              (lambda (y)
                                (format #t "~2d " (* x y)))
                              (iota 9 1))
                (newline))
              (iota 9 1))


(for-each-ext break1 next1
              (lambda (x)
                (for-each-ext break2 next2
                              (lambda (y)
                                (cond ((not (number? x)) (next1 x))
                                      ((not (number? y)) (next2 y))
                                      ((< x y) (break2 x))
                                      ((>= x 100) (break1 'done))
                                      (else (format #t "~2d " (* x y)))))
                              '(1 2 3 a 4 5 b 6 7 c 8 #\a 9 '() 10))
                (newline))
              '(#\x 1 2 3 x 4 5 6 y 7 8 9 10 z 100))

;; 簡易例外機構
(define *signals* '())

(define-syntax catch
  (syntax-rules (finally)
    [(_ (sig body ...) (finally follow ...))
     (let* ((signals-backup *signals*)
            (val (call/cc (lambda (k)
                            (set! *signals* (cons (cons 'sig k) *signals*))
                            body ...))))
       (set! *signals* signals-backup)
       follow ...
       val)]
    [(_ (sig body ...))
     (catch (sig body ...) (finally))]))

(define-syntax throw
  (syntax-rules ()
    [(_ sig val) ((cdr (assq 'sig *signals*)) val)]))

;; use sample
(define (div n d)
  (if (= d 0)
      (throw DivideZeroError
             (print #`"ERROR: Divide Zero Error Occured...\n divide ,n by ZERO!\n--------"))
      (/ n d)))

(define (percentage a b)
  (catch (DivideZeroError
          (print (* (div a b) 100.0) "%"))
         (finally
          (print "follow..."))))

(percentage 1 40)
(percentage 10 0)

;; co-routine
(use util.queue)

(define *tasks* (make-queue))

(define-syntax define-coroutine
  (syntax-rules ()
    [(_ (routine yield) body ...)
     (define (routine)
       (call/cc (lambda (return)
                  (define (yield)
                    (call/cc (lambda (cont)
                               (enqueue! *tasks* cont)
                               (return))))
                  body ...))
       ((dequeue! *tasks*)))]))

(define (coroutine-init! . rs)
  (set! *tasks* (make-queue))
  (for-each (lambda (r)
              (enqueue! *tasks* r))
            rs))

(define-coroutine (three yield)
  (let lp ()
    (print 'one)
    (yield)
    (print 'two)
    (yield)
    (print 'three)
    (yield)
    (lp)))

;; (three)


;; module
(add-load-path ".") ;; moduleがあるパスを*load-path*に追加
(require "module-sample")
;; (require "./module-sample") ; load-pathに追加しない場合はパス指定する必要がある
(import module-sample)

(use module-sample) ;; require,importの対象が同名であればuseが使える

(define x 1)
(addx 10) ; x が干渉しない


