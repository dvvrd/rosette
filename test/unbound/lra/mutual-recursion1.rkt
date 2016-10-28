#lang rosette/unbound

(current-bitwidth #f)

(define-symbolic n integer?)
(define x 0)
(define y 0)

(define/unbound (f n) (~> integer? integer?)
  (g n))

(define/unbound (g n) (~> integer? integer?)
  (set! x (add1 x))
  (h n))

(define/unbound (h n) (~> integer? integer?)
  (cond [(<= n 0) 0]
        [else
         (set! y (add1 y))
         (g (sub1 n))]))

(verify/unbound (assert (and (zero? (f n)) (equal? x y))))
