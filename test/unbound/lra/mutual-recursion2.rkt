#lang rosette/unbound

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic y integer?)

(define x 2)
(set! y 1)
(set! x 3)

(define/unbound (f n) (~> integer? integer?)
  (cond
    [(> n 0) (set! y (+ y n))
             (g (- n 1))]
    [else y]))

(define/unbound (g n) (~> integer? integer?)
  (set! x (add1 x))
  (set! y (+ y (f n)))
  n)

x
y
(f 5)
x
y

(verify/unbound (assert (and (>= (f n) 0) (>= y 0))))
(verify/unbound (assert (and (> (f n) 0) (> y 0))))
