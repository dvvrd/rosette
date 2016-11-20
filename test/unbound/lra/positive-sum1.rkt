#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic n m integer?)

; m emulates a non-negative random value
(define/unbound (positive-sum n) (~> integer? integer?)
  (cond
    [(= n 0) 0]
    [else (+ 1 m (positive-sum (- n 1)))]))

(define positive-sum1-tests
  (test-suite+
   "[unbound] Tests for lra/positive-sum1.rkt"

   (check-unsat
    (verify/unbound #:assume (assert (! (negative? m)))
                   #:guarantee (assert (>= (positive-sum n) n))))

   (check-sat
    (verify/unbound #:assume (assert (! (negative? m)))
                   #:guarantee (assert (> (positive-sum n) n))))))

(time (run-tests positive-sum1-tests))
