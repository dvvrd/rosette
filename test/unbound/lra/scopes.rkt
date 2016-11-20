#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic m integer?)
(define/unbound (f n) (~> integer? integer?)
  (define-symbolic k integer?)
  (if (>= k 0) (+ n m k) (+ n m)))

(define/unbound (g n) (~> integer? boolean?)
  (equal? n m))

(define-symbolic a integer?)

(define scopes-tests
  (test-suite+
   "[unbound] Tests for lra/scopes.rkt"

   (check-unsat
    (verify/unbound #:assume    (assert (> m 0))
                    #:guarantee (assert (> (f m) 0))))
   (check-sat
    (verify/unbound (assert (and (g m) (not (g a))))))))

(time (run-tests scopes-tests))
