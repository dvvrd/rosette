#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (g x y) x)
(define (twice f x y) (f (curry f x) y))
(define (neg x y) (- (x (void))))

; Expecting unsat
(verify/unbound
 #:assume (assert (>= n 0))
 #:guarantee (let ([z (twice neg (curry g n) (void))])
               (assert (>= z 0))))
