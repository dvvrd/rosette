#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(ite-compactification #t)
(merge-accuracy 10)
(define-symbolic x1 x2 x3 x4 x5 x6 integer?)

(define/unbound (f1 i n) (~> integer? integer? integer?)
  (if (< i n) (+ i (f1 (+ i 1) n)) 0))

(define/unbound (f2 i n1 n2 n3 n4 n5 n) (~> integer? integer? integer? integer? integer? integer? integer? integer?)
  (define x (if (and (>= i 0) (< i n1)) i 0))
  (if (< i n) (+ x (f2 (+ i 1) n1 n2 n3 n4 n5 n)) 0))

(define/unbound (f3 i n1 n2 n3 n4 n5 n) (~> integer? integer? integer? integer? integer? integer? integer? integer?)
  (define x (if (and (>= i n1) (< i n2)) i 0))
  (if (< i n) (+ x (f3 (+ i 1) n1 n2 n3 n4 n5 n)) 0))

(define/unbound (f4 i n1 n2 n3 n4 n5 n) (~> integer? integer? integer? integer? integer? integer? integer? integer?)
  (define x (if (and (>= i n2) (< i n3)) i 0))
  (if (< i n) (+ x (f4 (+ i 1) n1 n2 n3 n4 n5 n)) 0))

(define/unbound (f5 i n1 n2 n3 n4 n5 n) (~> integer? integer? integer? integer? integer? integer? integer? integer?)
  (define x (if (and (>= i n3) (< i n4)) i 0))
  (if (< i n) (+ x (f5 (+ i 1) n1 n2 n3 n4 n5 n)) 0))

(define/unbound (f6 i n1 n2 n3 n4 n5 n) (~> integer? integer? integer? integer? integer? integer? integer? integer?)
  (define x (if (and (>= i n4) (< i n5)) i 0))
  (if (< i n) (+ x (f6 (+ i 1) n1 n2 n3 n4 n5 n)) 0))

(define/unbound (f7 i n1 n2 n3 n4 n5 n) (~> integer? integer? integer? integer? integer? integer? integer? integer?)
  (define x (if (and (>= i n5) (< i n)) i 0))
  (if (< i n) (+ x (f7 (+ i 1) n1 n2 n3 n4 n5 n)) 0))

(time
   (verify/unbound #:assume    (assert (and (< x1 x2) (< x2 x3) (< x3 x4) (< x4 x5) (< x5 x6)))
                   #:guarantee (assert (let ([m1 (f1 0                x6)]
                                             [m2 (f2 0 x1 x2 x3 x4 x5 x6)]
                                             [m3 (f3 0 x1 x2 x3 x4 x5 x6)]
                                             [m4 (f4 0 x1 x2 x3 x4 x5 x6)]
                                             [m5 (f5 0 x1 x2 x3 x4 x5 x6)]
                                             [m6 (f6 0 x1 x2 x3 x4 x5 x6)]
                                             [m7 (f7 0 x1 x2 x3 x4 x5 x6)])
                                       (= m1 (+ m2 m3 m4 m5 m6 m7))))))