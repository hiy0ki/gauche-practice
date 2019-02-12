
(let* ((a (string->number (read-line)))
       (line2 (string-split (read-line) " "))
       (b (string->number (car line2)))
       (c (string->number (cadr line2)))
       (s (read-line)))
  (print (+ a b c) " " s))
