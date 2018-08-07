;; black jack

(define (create-card-from-simbol simbol numbers)
  (map (lambda (n)
         (cons simbol n))
       numbers))

(define (create-card-list simbols numbers)
  (if (null? simbols) '()
      (append (create-card-from-simbol (car simbols) numbers)
              (create-card-list (cdr simbols) numbers))))

(define (card-list)
  (create-card-list '(h k d s) (iota 13 1)))

;;; shuffle
;; カードのリストの長さを取得
;; その長さ内のランダムな数字を取る
;; その数字を要素としてカードのリストから一つ取り出し出力用のリストに追加する
;; カードのリストがなくなるまで繰り返す
(use srfi-27)
(define (card-shuffle card-list :optional (deck '()))
  (if (null? card-list) deck
      (let* ((key (random-integer (length card-list)))
             (val (list-ref card-list key)))
        (card-shuffle (delete val card-list) (append (list val) deck)))))

;; deck
(define-class <deck> ()
  ((deck :init-value '() :init-keyword :deck :accessor deck-of)))

(define-method create-deck ((d <deck>))
  (set! (deck-of d) (card-shuffle (card-list))))

(define-method draw-card ((d <deck>))
  (pop! (deck-of d)))

;; player
(define-class <player> ()
  ((name :init-value 'player :init-keyword :name :accessor name-of)
   (hands :init-value '() :init-keyword :hands :accessor hands-of)
   (point :init-value 0 :init-keyword :point :accessor point-of)
   (stay? :init-value #f :init-keyword :stay? :accessor stay?-of)))

(define-method add-hands ((p <player>) card)
  (set! (hands-of p) (append (list card) (ref p 'hands)))
  (add-point p))

;; todo: j q k を点数丸める Aの扱いも追加する
(define-method add-point ((p <player>))
  (set! (point-of p) (calc-point p)))

(define-method calc-point ((p <player>))
                    (+ (point-of p)
                       (let ((card-num (cdr (car (hands-of p)))))
                         (cond ((null? card-num) 0)
                               ((> card-num 10) 10) ; j q k
                               ((eq? card-num 1) ; A
                                (if (> (point-of p) 10)
                                    1
                                    11))
                               (else card-num)))))

(define-method open-hands/mask ((p <player>))
  (append '("***") (cdr (hands-of p))))

(define (show-field com usr)
  (print (name-of com) ":  " (open-hands/mask com))
  (print (name-of usr) ":  " (hands-of usr)))

(define (show-result p1 p2)
  (print "The winner is " (name-of (winner p1 p2)))
  (print (name-of p1) ":  point: " (point-of p1) " hands: " (hands-of p1))
  (print (name-of p2) ":  point: " (point-of p2) " hands: " (hands-of p2)))

;; add if over 21
(define (winner p1 p2)
  (if (>= (point-of p1) (point-of p2))
      p1
      p2))

(define (over-point-limit? point)
  (if (> point 21) #t #f))

;; game loop
;; com,user,deckはすでにある
;; userのstay?
;; if user selected false, draw card
;; com stay?
;; if com selected false, draw card
;; next user turn

(define (game com usr deck)
  (show-field com usr)
  (display "stay?[y\/n]\n> ")
  (let ((usr-stay? (read)))
    (cond ((and
            (if (stay?-of usr) #f #t)
            (eq? usr-stay? 'n))
           (print "usr draws card")
           (add-hands usr (draw-card deck)))
          (else (set! (stay?-of usr) #t)))
    (cond ((com-stay? com)
           (print "com draws card")
           (add-hands com (draw-card deck)))
          (else (set! (stay?-of com) #t)))
    (print "end")))

;; TODO もっとスッキリかけるはず
(define (com-stay? com)
  (if (or (> (point-of com) 17)
          (stay?-of com))
      #t
      #f))

(define (next-turn? com usr)
  (if (and (stay?-of com) (stay?-of usr))
      #f
      #t))


;;; game
;; computer,userを作る
;; deckを作る
;; cardを配る
;; playerの順番をどうやって制御するか
(define (game-test)
  (let ((com (make <player> :name 'computor))
        (usr (make <player> :name 'you))
        (deck (make <deck>)))
    (create-deck deck)
    (add-hands com (draw-card deck))
    (add-hands usr (draw-card deck))
    (add-hands com (draw-card deck))
    (add-hands usr (draw-card deck))
    (let loop ((c com)
               (u usr)
               (d deck))
      (game c u d)
      (when (next-turn? c u)
            (loop c u d)))
    (show-field com usr)
    (show-result com usr)))

;; todo computerに依存させたほうが良さそう。
(define (next-draw? p)
  (or (<= (point-of p) 17)
      (<= (point-of p) 21)))

