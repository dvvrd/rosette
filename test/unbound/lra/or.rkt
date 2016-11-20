#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define/unbound (id/boolean c) (~> boolean? boolean?)
  c)

(define/unbound (id/integer z) (~> integer? integer?)
  z)

(define-symbolic a b boolean?)
(define-symbolic x y integer?)

(define or-tests
  (test-suite+
   "[unbound] Tests for lra/or.rkt"

   (check-unsat
    (verify/unbound #:assume (assert (and a (not b)))
                    #:guarantee (assert (or (id/boolean a) (id/boolean b)))))

   (check-unsat
    (verify/unbound #:assume (assert (and (positive? x) (positive? y)))
                    #:guarantee (assert (positive? (+ (id/integer x) (id/integer y))))))

   (check-sat
    (verify/unbound (assert (positive? (+ (id/integer x) (id/integer y))))))

   (check-sat
    (verify/unbound #:assume (assert (and (not (negative? x)) (not (negative? y))))
                    #:guarantee (assert (positive? (+ (id/integer x) (id/integer y))))))))

(time (run-tests or-tests))
