#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic m n integer?)

(define/unbound (inc n) (~> integer? integer?)
  (+ n 1))

(define/unbound (dec n) (~> integer? integer?)
  (- n 1))

(define/unbound (inc->inc->dec n) (~> integer? integer?)
  (dec (inc (inc n))))

(define inc-tests
  (test-suite+
   "[unbound] Tests for lra/inc.rkt"

   (check-unsat
    (verify/unbound #:assume    (assert (equal? n m))
                    #:guarantee (assert (> (inc->inc->dec n) m))))

   (check-sat
    (verify/unbound #:assume    (assert (equal? n (add1 m)))
                    #:guarantee (assert (= (inc->inc->dec n) m))))

   (check-unsat
    (verify/unbound #:assume    (assert (equal? n (sub1 m)))
                    #:guarantee (assert (= (inc->inc->dec n) m))))))

(time (run-tests inc-tests))
