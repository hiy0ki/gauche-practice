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

;; 数字の入力
(define (input-number)
  (display "please input integer(1-100)\n> ")
  (let ((data (read)))
    (cond ((not (integer? data))
           (display data)
           (display "is not integer\n")
           (input-number))
          ((<= 1 data 100) data)
          (else
           (display "range error\n")
           (input-number)))))

;; 数あてゲーム
(define (game answer)
  (let loop ((count 1))
    (if (< 7 count)
        (display "Game Over\n")
        (let ((data (input-number)))
          (cond ((= data answer)
                 (display "Congratulation!!\n"))
                ((< data answer)
                 (display "Number is greater than")
                 (display data)
                 (newline)
                 (loop (+ count 1)))
                (else
                 (display "Number is less than")
                 (display data)
                 (newline)
                 (loop (+ count 1))))))))

;; gameの実行
(define (play)
  (srand (sys-time))
  (let loop ((answer (make-number 100)))
    (game answer)
    (display "Try Again?(y or n)\n> ")
    (if (eq? 'y (read))
        (loop (make-number 100))
        (display "Thanks!\n"))))
           

