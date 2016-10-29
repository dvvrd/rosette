#lang rosette/unbound

; TODO: interesting observation: if we increment x on 1 each iteration
; and try to prove x = y after all spacer can't accomplish it, though
; it solves current query (intuitively harder one)!

(current-bitwidth #f)

(define-symbolic n integer?)
(define x 0)
(define y 0)

(define/unbound (f n) (~> integer? integer?)
  (g n))

(define/unbound (g n) (~> integer? integer?)
  (set! x (+ x 2))
  (h n))

(define/unbound (h n) (~> integer? integer?)
  (set! y (add1 y))
  (cond [(<= n 0) 0]
        [else (g (sub1 n))]))

(verify/unbound (assert (and (zero? (f n)) (equal? x (+ y y)))))
(verify/unbound (assert (and (zero? (f n)) (equal? x (* 2 y)))))
