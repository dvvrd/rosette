#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic i n n1 n2 integer?)

(define/unbound (f n) (~> integer? integer?)
  (if (<= n 0) n (+ n (f (- n 1)))))

(define/unbound (g n) (~> integer? integer?)
  (if (<= n 1) n (+ n (g (- n 1)))))

(time
   (verify/unbound #:assume    (assert (= n1 n2))
                   #:guarantee (assert
                                 (let ([m1 (f n1)] [m2 (g n2)])
                                 (= m1 m2)))))

