#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic n m integer?)

(define/unbound (gcd n m) (~> integer? integer? integer?)
  (cond [(< n m) (gcd m n)]
        [(zero? n) m]
        [(zero? m) n]
        [else (gcd m (- n m))]))

(define gcd-tests
  (test-suite+
   "[unbound] Tests for lra/gcd.rkt"

   (check-unsat
    (verify/unbound #:assume (assert (and (> n 0) (> m 0)))
                    #:guarantee (assert (> (gcd m n) 0))))

   (check-sat (verify/unbound (assert (> (gcd m n) 0))))))

(time (run-tests gcd-tests))
