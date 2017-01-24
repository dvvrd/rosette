#lang rosette/unbound

(dbg-level 0)
(define-symbolic x y z integer?)

(define/unbound max (max2) (x y z) (~> integer? integer? integer? integer?)
  (max2 (max2 x y) z))

(define/unbound (f x y) (~> integer? integer? integer?)
  (if (>= x y) x y))

(time
 (let ([m (max f x y z)])
  ; Expecting unsat
   (verify/unbound (assert (= (f x m) m)))))
