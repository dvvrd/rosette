#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(merge-accuracy 4)

(define-symbolic xs (listof integer?))
(define-symbolic n integer?)

(define/typed (positive? x) (~> integer? boolean?)
  (> x 0))
(define/typed (>n x) (~> integer? boolean?)
  (> x n))
(define/typed (-n x) (~> integer? integer?)
  (- x n))

(define read-deps-tests
  (test-suite+
   "[unbound] Tests for lists/read-dependencies.rkt"
   
   (check-unsat
    (verify/unbound #:assume (assert (andmap >n xs))
                #:guarantee (assert (andmap positive? (map -n xs)))))

   (check-sat
    (verify/unbound #:assume (assert (andmap >n xs))
                #:guarantee (assert (andmap positive? (map -n (map -n xs))))))
))

(time (run-tests read-deps-tests))
