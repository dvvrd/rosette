#lang rosette/unbound

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic f integer?)

(define x 2)
(set! f 1)
(set! x 3)

(define/unbound (fact n) (~> integer? integer?)
  (set! x (add1 x))
  (cond
    [(> n 0) (set! f (* f n))
             (fact (- n 1))]
    [else f]))

(define/unbound (g n) (~> integer? integer?)
  (set! f (+ f 7))
  n)


x
f
(fact 5)
x
f

(verify/unbound (assert (and (> (fact n) 0) (> f 0))))
