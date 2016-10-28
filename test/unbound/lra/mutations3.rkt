#lang rosette/unbound

(current-bitwidth #f)

(define-symbolic n integer?)

(define-symbolic sum1 sum2 sum3 integer?)

(set! sum1 0)
(set! sum2 0)
(set! sum3 0)

(define/unbound (sum n) (~> integer? integer?)
  (define-symbolic m integer?)
  (set! sum1 (+ sum1 m))

  (define-symbolic m1 boolean?)

  (set! sum2 (if m1 (+ sum2 m) sum2))
  (set! sum3 (if (not m1) (+ sum3 m) sum3))

  (if (>= n 0) (sum (- n 1)) 0)
)

(sum n)

(verify/unbound (assert (= sum1 (+ sum2 sum3))))
