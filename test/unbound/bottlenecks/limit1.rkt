#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic i n n1 n2 n3 n4 n5 n6 integer?)

; does n't work?

(define/unbound (f n) (~> integer? integer?)
  (if (<= n 1) n (+ n (f (- n 1)))))

(define/unbound (g n) (~> integer? integer?)
  (if (<= n 1) n (+ n (- n 1) (g (- n 2)))))

(time
   (verify/unbound #:assume    (assert (= n1 n2))
                   #:guarantee (assert
                                 (let ([m1 (f n1)] [m2 (g n2)])
                                 (= m1 m2)))))

