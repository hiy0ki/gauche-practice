; 線形合同法による乱数の生成
(define *seed* 1)

(define (srand x)
  (set! *seed* x))

(define (irand)
  (set! *seed* (modulo (+ (* 69069 *seed*) 1) #x100000000))
  *seed*)

(define (random)
  (* (/ 1.0 #x100000000) (irand)))

(define (get-time)
  (sys-time))

;; 1 to n の乱数生成
(define (make-number n)
  (+ (modulo (quotient (irand) #x10000) n) 1))



