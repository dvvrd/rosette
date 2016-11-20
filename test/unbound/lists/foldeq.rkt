#lang rosette/unbound

(define-symbolic xs (listof integer?))
(define/typed (+/typed x y)
  (~> integer? integer? integer?)
  (+ x y))

(define a (foldl +/typed 0 xs))
(define b (foldl +/typed 0 xs))
(verify/unbound (assert (= a b)))
