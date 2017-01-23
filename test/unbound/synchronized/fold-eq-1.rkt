#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)

(define-symbolic n integer?)

(define-symbolic xs (listof integer?))

(define/typed (+/typed x y)
  (~> integer? integer? integer?)
  (+ x y))

(define/typed (-/typed x y)
  (~> integer? integer? integer?)
  (- y x))

(define/typed (+2x/typed x y)
  (~> integer? integer? integer?)
  (+ y x x))

(define a (foldl +/typed 0 xs))
(define b (foldl +/typed 0 xs))
(define c (foldl +2x/typed 0 xs))
(define d (foldl +/typed n xs))
(define e (foldl -/typed 0 xs))

   (time
     (verify/unbound (assert (= a b))))
