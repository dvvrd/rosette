#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic xs (listof integer?))

(define/typed (add1 x) (~> integer? integer?) (+ x 1))
(define/typed (+/typed x y) (~> integer? integer? integer?) (+ x y))

(define map-fold-tests
  (test-suite+
   "[unbound] Tests for lists/map-fold.rkt"

   (check-unsat
    (verify/unbound
     (assert
      (= (foldl +/typed 0 (map add1 xs))
         (+ (foldl +/typed 0 xs)
            (length xs))))))

      (check-sat
       (verify/unbound
        (assert
         (= (foldl +/typed 1 (map add1 xs))
            (+ (foldl +/typed 0 xs)
               (length xs))))))))

(time (run-tests map-fold-tests))
