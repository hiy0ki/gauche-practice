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
;; �����ɤΥꥹ�Ȥ�Ĺ�������
;; ����Ĺ����Υ�����ʿ�������
;; ���ο��������ǤȤ��ƥ����ɤΥꥹ�Ȥ����ļ��Ф������ѤΥꥹ�Ȥ��ɲä���
;; �����ɤΥꥹ�Ȥ��ʤ��ʤ�ޤǷ����֤�
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
  ((name :init-value 'player :init-keyword :name :accessor name-of)
   (hands :init-value '() :init-keyword :hands :accessor hands-of)
   (point :init-value 0 :init-keyword :point :accessor point-of)
   (stay? :init-value #f :init-keyword :stay? :accessor stay?-of)))

(define-method add-hands ((p <player>) card)
  (set! (hands-of p) (append (list card) (ref p 'hands)))
  (add-point p card))

;; todo: j q k �������ݤ�� A�ΰ������ɲä���
(define-method add-point ((p <player>) card)
  (set! (point-of p) (calc-point p card)))

;; todo: case 1(A)
(define (calc-point ((p <player>) card)
  (+ (point-of p)
     (if (> (cdr card) 10)
         10
         (cdr card))))

;; �껥���������
(define-method open-hands ((p <player>))
  (hands-of p))

(define-method open-hands/mask ((p <player>))
  (append '("***") (cdr (hands-of p))))

(define (show-field p1 p2)
  (print (name-of p1) ":  " (open-hands/mask p1))
  (print (name-of p2) ":  " (open-hands/mask p2)))

(define (show-result p1 p2)
  (print "The winner is " (name-of (winner p1 p2)))
  (print (name-of p1) ":  point: " (point-of p1) " hands: " (open-hands p1))
  (print (name-of p2) ":  point: " (point-of p2) " hands: " (open-hands p2)))

;; add if over 21
(define (winner p1 p2)
  (if (>= (point-of p1) (point-of p2))
      p1
      p2))

;;; game
;; computer,user����
;; deck����
;; card���ۤ�
;; player�ν��֤�ɤ���ä����椹�뤫
(define (game)
  (let ((com (make <player> :name 'computor))
        (usr (make <player> :name 'you))
        (deck (make <deck>)))
    (create-deck deck)
    (add-hands com (draw-card deck))
    (add-hands usr (draw-card deck))
    (add-hands com (draw-card deck))
    (add-hands usr (draw-card deck))
    (show-field com usr)
    (show-result com usr)))


(define (next-draw? p)
  (<= (point-of p) 21))

(define (input-user-action)
  (display "next action? draw=d,stay=s\n>")
  (let ((in (read)))
    (cond
     ((equal? in 'd)
      (print "draw")
      (add-hands *usr* (draw-card deck)))
     ((equal? in 's)
      (print "stay")
      (set! (stay?-of *usr*) #t))
     (else
      (print "other")))))

(define (game-loop)
  (let loop ((np *start-player*))
      (if (and (stay?-of *usr*) (stay?-of *com*))
          (show-result *com* *usr*)
          (loop (next-player))))))

(define *com* (make <player> :name 'computer))
(define *usr* (make <player> :name 'you))

(define (input-com-action)
  (print "com's turn")
  (if (next-draw?)
      (add-hands *com* (draw-card deck))
      (set! (stay?-of *com*) #t)))

(define (turn-end? p1 p2)
  (and (stay?-of p1) (stay?-of p2)))


