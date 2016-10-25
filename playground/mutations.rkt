#lang rosette/unbound

;(define-symbolic m k integer?)
;(define-symbolic n integer?)
;
;(define x 1)
;(set! x 3)
;(set! n 100500)
;
;(define (set-n a b)
;  (set! a b))
;
;(define/unbound (f v) (~> integer? integer?)
;  (printf "X: ~a\n" x)
;  (printf "N: ~a\n" n)
;  (set-n n v))
;
;(define-symbolic n1 integer?)
;
;n
;
;(if (< k 0)
;    (set! n m)
;    (set! n k))
;
;(if (< n1 0)
;    (set! n1 m)
;    (set! n1 k))
;
;n
;n1

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic f integer?)

(define x 2)
(set! f 1)
(set! x 3)

(define/unbound (fact n) (~> integer? integer?)
  ;(set! x (add1 x))
  (cond
    [(> n 0) (set! f (* f n))
             (fact (- n 1))]
    [else f]))

(define/unbound (g n) (~> integer? integer?)
  (set! f (add1 f))
  n)


x
f
(fact 5)
x
f

(verify/unbound (assert (and (> (fact n) 0) (> f 0))))
