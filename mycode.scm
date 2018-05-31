;; mycode.scm
(define (last-pair2 lis)
  (if (pair? (cdr lis))
      (last-pair2 (cdr lis))
      lis))


