(use srfi-1)

(define (check-num x)
  (cond [(= (modulo x 15) 0) 'FizzBuzz]
        [(= (modulo x 5) 0) 'Buzz]
        [(= (modulo x 3) 0) 'Fizz]
        [else x]))

(define (fizz-buzz check max)
  (map check (iota max 1)))


(define (list?2 obj)
  (or (null? obj)
      (and (pair? obj) (list? (cdr obj)))))

(fold + 0 '(1 2 3 4 5))

(define (sum-of-numbers lis)
  (fold + 0 lis))

(define (product-of-numbers lis)
  (fold * 1 lis))

(define (max-number lis)
  (define (pick-greater a b)
    (if (> a b) a b))
  (fold pick-greater -inf.0 lis))





        
