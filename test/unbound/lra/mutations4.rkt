#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic n integer?)

(define-symbolic part integer?)

(define-symbolic sum1 sum2 sum3 integer?)

(set! part 1)

(set! sum1 0)
(set! sum2 0)
(set! sum3 0)

(define/unbound (sum n) (~> integer? integer?)
  (define-symbolic m integer?)
  (set! sum1 (+ sum1 m))

  (define-symbolic m1 boolean?)
  (set! part (if (and m1 (= part 1)) 2 part))

  (set! sum2 (if (= part 1) (+ sum2 m) sum2))
  (set! sum3 (if (= part 2) (+ sum3 m) sum3))

  (if (>= n 0) (sum (- n 1)) 0)
)

(sum n)

(define mutations4-tests
  (test-suite+
   "[unbound] Tests for lra/mutations4.rkt"

   (check-unsat
    (verify/unbound (assert (= sum1 (+ sum2 sum3)))))
   (check-sat
    (verify/unbound (assert (= (abs sum1) (+ sum2 sum3)))))))

(time (run-tests mutations4-tests))
