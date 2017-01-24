#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (f x g)
  (g (+ x 1)))

(define (h y)
  (assert (positive? y)))

; Expecting unsat
(time
 (verify/unbound #:assume (assert (>= n 0))
                 #:guarantee (assert (f n h))))
