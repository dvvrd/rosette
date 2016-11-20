#lang rosette/unbound

; Expecting unsat

(define m 0)
(define-symbolic n integer?)

(define/unbound (f x) (~> integer? integer?)
  (set! m 1)
  (+ x m))

(define/unbound (g x) (~> integer? integer?)
  (+ x m))

(define fv (f n))
(define gv (g n))

(verify/unbound (assert (= fv gv)))
