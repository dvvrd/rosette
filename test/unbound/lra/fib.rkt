#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic m integer?)

(define/unbound (fib n) (~> integer? integer?)
  (if (< n 2) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define fib-tests
  (test-suite+
   "[unbound] Tests for lra/fib.rkt"

   (check-unsat (verify/unbound (assert (> (fib m) 0))))
   (check-sat (verify/unbound (assert (> (fib m) 1))))))

(time (run-tests fib-tests))
