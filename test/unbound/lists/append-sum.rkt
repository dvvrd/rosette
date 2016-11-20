#lang rosette/unbound

(define/typed (+/typed x y) (~> integer? integer? integer?)
  (+ x y))

(define-symbolic xs ys (listof integer?))
(define zs (append xs ys))

(define a (foldl +/typed 0 zs))
(define b (+ (foldl +/typed 0 xs)
             (foldl +/typed 0 ys)))

(verify/unbound (assert (= a b)))
