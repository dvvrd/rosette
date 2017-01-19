#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (copy x) (~> integer? integer?)
  (cond [(= x 0) 0]
        [else (+ 1 (copy (- n 1)))]))

; Expecting unsat
(verify/unbound (assert (= (copy (copy n)) n)))
