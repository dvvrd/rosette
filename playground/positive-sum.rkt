#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic n m integer?)

; m emulates a non-negative random value
(define/unbound (positive-sum n) (~> integer? integer?)
  (cond
    [(= n 0) 0]
    [else (+ 1 m (positive-sum (- n 1)))])
)

(verify/unbound #:assume (assert (! (negative? m)))
                #:guarantee (assert (>= (positive-sum n) n)))
