#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (add x y) (+ x y))
(define/unbound (sum n) (~> integer? integer?)
  (cond [(<= n 0) 0]
        [else (+ n (sum (- n 1)))]))

; Expecting unsat
(time
 (verify/unbound (assert (<= n (sum n)))))
