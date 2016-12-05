#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic xs (listof integer?))
(define a 0)

(define/typed (+/typed x y)
  (~> integer? integer? integer?)
  (begin0
    (+ y x)
    (set! a (add1 a))))

(foldl +/typed 0 xs)

(define prev 0)

(define/typed (sorted/typed x y)
  (~> integer? boolean? boolean?)
  (begin0
    (if (and (>= x prev) y) #t #f)
    (set! prev x)) 
)

(define/typed (min/typed x y)
  (~> integer? integer? integer?)
  (min y x))

(define fs (car xs))
(define mn (foldl min/typed 0 xs))

(define srt (foldl sorted/typed #t xs))

(verify/unbound (assert (implies srt (= fs mn))))

(define foldmut-tests
  (test-suite+
   "[unbound] Tests for lists/fold-mutations.rkt"

   (check-unsat
    (verify/unbound (assert (= a (length xs)))))

   (check-sat
    (verify/unbound (assert (= (add1 a) (length xs)))))))

(time (run-tests foldmut-tests))
