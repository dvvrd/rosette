#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define/unbound (f n) (~> integer? integer?)
  (+ n 1))
(define/unbound (g n) (~> boolean? boolean?)
  (not n))

(define-symbolic b boolean?)
(define-symbolic i integer?)

(define arg-types-tests
  (test-suite+
   "[unbound] Tests for lra/types.rkt"

   (check-unsat
    (verify/unbound (assert (and (> (f i) i) (or (g b) b)))))

   (check-sat
    (verify/unbound (assert (and (> (f i) i) (and (g b) b)))))))

(time (run-tests arg-types-tests))
