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

;; todo: j q k �������ݤ�� A�ΰ������ɲä���
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
(define (game-test)
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

;; todo computer�˰�¸�������ۤ����ɤ�������
(define (next-draw? p)
  (or (<= (point-of p) 17)
      (<= (point-of p) 21)))

;; todo user��������������ۤ�����������

#;(define (input-user-action)
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

#;(define (game-loop)
  (let loop ((np *start-player*)
      (if (and (stay?-of *usr*) (stay?-of *com*))
          (show-result *com* *usr*)
          (loop (next-player)))))

(define *deck* (make <deck>))
(define *com* (make <player> :name 'computer))
(define *usr* (make <player> :name 'you))

(define (input-com-action)
  (print "com's turn")
  (if (next-draw?)
      (add-hands *com* (draw-card deck))
      (set! (stay?-of *com*) #t)))

(define (turn-end? p1 p2)
  (and (stay?-of p1) (stay?-of p2)))


