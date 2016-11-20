#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic n sum1 integer?)

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

(define mutations2-tests
  (test-suite+
   "[unbound] Tests for lra/mutations2.rkt"

   (check-unsat
    (verify/unbound (assert (>= sum1 0))))
   (check-sat
    (verify/unbound (assert (> sum1 0))))))

(time (run-tests mutations2-tests))
