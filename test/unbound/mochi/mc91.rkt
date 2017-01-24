#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (mc91 x) (~> integer? integer?)
  (cond [(> x 100) (- x 10)]
        [else (mc91 (mc91 (+ x 11)))]))

; Expecting unsat
(time
 (verify/unbound #:assume (assert (<= n 101))
                 #:guarantee (assert (= (mc91 n) 91))))
