#lang rosette/unbound

(current-bitwidth #f)
(ite-compactification #t)
(dbg-level 0)

(define-symbolic x1 x2 y1 y2 integer?)

(define/unbound (div1 x y) (~> integer? integer? integer?)
   (if (< x y) 0
      (+ 1 (div1 (- x y) y))))

(define/unbound (f1 n) (~> integer? integer? )
  (if (>= n 10) 
      (+ 1 (f1 (div1 n 10)))
      1))

(time
   (verify/unbound #:assume    (assert (> x1 x2) )
                   #:guarantee (assert
                                 (let ([z1 (f1 x1)] [z2 (f1 x2)])
                                 (>= z1 z2)))))


