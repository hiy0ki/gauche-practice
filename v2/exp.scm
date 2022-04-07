(define funcs (lambda (x y)
                (+ x y y)))


(define (if-sam a b c)
  (if (< a b c)
      (display "ok")
      (display "ng")))

(define (fact n)
  (define (fact2 m res)
    (if (= m 0)
        res
        (fact2 (- m 1) (+ m res))))
  (fact2 n 0))

(define (fact2 n)
  (if (= n 0)
      1
      (* n (fact2 (- n 1)))))

(define (retrieve res n)
  (if (zero? n)
      (car res)
      (retrieve (cdr res) (- n 1))))

(define (1- x)
  (- x 1))

(define (pow x y)
  (if (zero? y)
      1
      (* x (pow x (1- y)))))


