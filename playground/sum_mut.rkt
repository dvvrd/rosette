#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic n integer?)

(define-symbolic sum1 integer?)

(set! sum1 0)

(define/unbound (sum n) (~> integer? integer?)
  (define-symbolic m integer?)
  (set! sum1
     (if (>= m 0)
         (+ sum1 m)
         sum1)
  )
  (if (>= n 0) (sum (- n 1)) 0)
)

(sum n)

(verify/unbound (assert (>= sum1 0)))
