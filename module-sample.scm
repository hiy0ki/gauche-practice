(define-module module-sample
  (export addx))
(select-module module-sample)

(define x 100)

(define (addx n)
  (+ n x))

(provide "module-sample")


;; debug
(debug-print-width)

(d d)

(describe)

(d <generic>)

(ref <generic> 'name)

(class-of <generic>)
(class-of ref)

(all-modules)

(current-module)


