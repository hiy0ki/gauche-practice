;; -*- coding: utf-8 -*-
;; mycode-test.scm

(use gauche.test)
(test-start "mycode.scm")

(load "./mycode")

(test* "last-pair2 #1" '(3) (last-pair2 '(1 2 3)))
(test* "last-pair2 #2" '(1) (last-pair2 '(1)))

(test-section "section2")

(test* "last-pair2 #3" '(2 . 3) (last-pair2 '(1 2 . 3)))

(test-end)

