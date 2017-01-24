#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (succ x) (+ x 1))
(define/unbound repeat (f) (n s) (~> integer? integer? integer?)
  (if (zero? n)
      s
      (f (repeat f (- n 1) s))))

; Expecting sat
(time
 (verify/unbound (assert (> (repeat succ n 0) n))))
