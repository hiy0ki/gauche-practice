;; black jack
(define *card-simbols* '(h k d s))
(define *card-numbers* (iota 13 1))

(define (merge-card simbol numbers)
  (map (lambda (n)
         (cons simbol n))
       numbers))

(define (create-card-list simbols numbers)
  (if (null? simbols) '()
      (append (merge-card (car simbols) numbers) (create-card-list (cdr simbols) numbers))))

(define (card-list)
  (create-card-list *card-simbols* *card-numbers*))

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

#;(define (create-deck)
  (card-shuffle (card-list)))

;; deck
(define-class <deck> ()
  ((deck :init-value '() :init-keyword :deck :accessor deck-of)))

(define-method create-deck ((d <deck>))
  (set! (deck-of d) (card-shuffle (card-list))))

(define-method draw-card ((d <deck>))
  (pop! (deck-of d)))

;; player
(define-class <player> ()
  ((name :init-value "" :init-keyword :name :accessor name-of)
   (hands :init-value '() :init-keyword :hands :accessor hands-of)
   (point :init-value 0 :init-keyword :point :accessor point-of)))

(define-method add-hands ((p <player>) card)
  (set! (hands-of p) (append (list card) (ref p 'hands)))
  (add-point p card))

;; todo: j q k を点数丸める Aの扱いも追加する
(define-method add-point ((p <player>) card)
  (set! (point-of p) (+ (point-of p) (cdr card))))

;; 手札を公開する
(define-method open-hands ((p <player>))
  (hands-of p))

(define-method open-hands/mask ((p <player>))
  (append '("***") (cdr (hands-of p))))

(define (show-field p1 p2)
  (print (name-of p1) ":  " (open-hands/mask p1))
  (print (name-of p2) ":  " (open-hands/mask p2)))

(define (show-result p1 p2)
  (print (name-of (winner p1 p2)) " is win!")
  (print (name-of p1) ":  point: " (point-of p1) " hands: " (open-hands p1))
  (print (name-of p2) ":  point: " (point-of p2) " hands: " (open-hands p2)))

(define (winner p1 p2)
  (if (>= (point-of p1) (point-of p2))
      p1
      p2))

;;; game
;; computer,userを作る
;; deckを作る
;; cardを配る
;; playerの順番をどうやって制御するか
(define (game)
  (let ((com (make <player> :name "com"))
        (usr (make <player> :name "usr"))
        (deck (make <deck>)))
    (create-deck deck)
    (add-hands com (draw-card deck))
    (add-hands usr (draw-card deck))
    (add-hands com (draw-card deck))
    (add-hands usr (draw-card deck))
    (show-field com usr)
    (show-result com usr)))

    
             

