#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x1 x2 integer?)

(define/unbound (f1 i n) (~> integer? integer? integer?)
  (if (< i n) (+ i (f1 (+ i 1) n)) 0))

(define/unbound (f2 i n1 n2) (~> integer? integer? integer? integer?)
  (define x (if (and (>= i 0) (< i n1)) i 0))
  (if (< i n2) (+ x (f2 (+ i 1) n1 n2)) 0))

(define/unbound (f3 i n1 n2) (~> integer? integer? integer? integer?)
  (define x (if (and (>= i n1) (< i n2)) i 0))
  (if (< i n2) (+ x (f3 (+ i 1) n1 n2)) 0))

(time
   (verify/unbound #:assume    (assert (< x1 x2))
                   #:guarantee (assert
                                 (let ([m1 (f1 0 x2)] [m2 (f2 0 x1 x2)] [m3 (f3 0 x1 x2)])
                                 (= m1 (+ m2 m3))))))

