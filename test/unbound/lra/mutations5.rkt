#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define m 0)
(define-symbolic n integer?)

(define/unbound (f x) (~> integer? integer?)
  (set! m 1)
  (+ x m))

(define/unbound (g x) (~> integer? integer?)
  (+ x m))

(define fv (f n))
(define gv (g n))

(define mutations5-tests
  (test-suite+
   "[unbound] Tests for lra/mutations5.rkt"

   (check-unsat
    (verify/unbound (assert (= fv gv))))
   (check-sat
    (verify/unbound (assert (= (add1 fv) gv))))))

(time (run-tests mutations5-tests))
