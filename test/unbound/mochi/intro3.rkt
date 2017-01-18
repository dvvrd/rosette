#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (f x g)
  (g (+ x 1)))

(define (h z y)
  (assert (> y z)))

; Expecting unsat
(verify/unbound #:assume (assert (>= n 0))
                #:guarantee (assert (f n (curry h n))))
