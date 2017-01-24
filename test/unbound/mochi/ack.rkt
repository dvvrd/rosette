#lang rosette/unbound

(dbg-level 0)
(define-symbolic m n integer?)

(define/unbound (ackerman n m) (~> integer? integer? integer?)
  (cond [(zero? m) (add1 n)]
        [(zero? n) (ackerman (sub1 m) 1)]
        [else (ackerman (sub1 m) (ackerman m (sub1 n)))]))

; Expecting unsat
(time
 (verify/unbound #:assume (assert (and (>= m 0) (>= n 0)))
                 #:guarantee (assert (>= (ackerman m n) n))))
