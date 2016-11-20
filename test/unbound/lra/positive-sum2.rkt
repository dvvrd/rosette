#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define/unbound (f n) (~> integer? integer?)
  (cond [(> n 0)
         (define-symbolic k integer?)
         (+ (abs k) (f (sub1 n)))]
        [else 0]))

(define-symbolic m integer?)

(define positive-sum2-tests
  (test-suite+
   "[unbound] Tests for lra/positive-sum2.rkt"

   (check-unsat
    (verify/unbound (assert (>= (f m) 0))))
   (check-sat
    (verify/unbound (assert (> (f m) 0))))))

(time (run-tests positive-sum2-tests))
