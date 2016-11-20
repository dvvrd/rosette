#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic xs (listof integer?))

(define/unbound (+/abs x y) (~> integer? integer? integer?)
  (+ (abs x) y))

(define lst (if (< n 0) (list 1 2 3) xs))

(define union-tests
  (test-suite+
   "[unbound] Tests for lists/union.rkt"

   (check-unsat
    (verify/unbound (assert (>= (foldl +/abs 0 lst) 0))))
   (check-sat
    (verify/unbound (assert (>  (foldl +/abs 0 lst) 0))))))

(time (run-tests union-tests))
