
(define (pythagoras x y)
  (sqrt (+ (* x x) (* y y))))

(define (list2 . lis)
  (define (in-lis l res)
    (if (null? l) ;; null? は空リストであれば真値、そうでなければ偽値を返す
        (reverse res)
        (in-lis (cdr l) (cons (car l) res))))
  (in-lis lis '()))


(define (max-number lis)
  (define (pick-greater a b)
    (if (> a b) a b))
  (if (null? lis)
      (error "max-number needs at least one number.")
      (fold pick-greater (car lis) (cdr lis))))




