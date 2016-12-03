#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic xs (listof integer?))
(define a 0)

(define/typed (+/typed x y)
  (~> integer? integer? integer?)
  (begin0
    (+ y x)
    (set! a (add1 a))))

(foldl +/typed 0 xs)

(define foldmut-tests
  (test-suite+
   "[unbound] Tests for lists/fold-mutations.rkt"

   (check-unsat
    (verify/unbound (assert (= a (length xs)))))

   (check-sat
    (verify/unbound (assert (= (add1 a) (length xs)))))))

(time (run-tests foldmut-tests))
