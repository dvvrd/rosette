#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(ite-compactification #t)
(define-symbolic n integer?)

(define/unbound (fib n) (~> integer? integer?)
  (if (< n 2) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define/unbound (fib-swap n) (~> integer? integer?)
  (if (< n 2) 1
      (+ (fib-swap (- n 2))
         (fib-swap (- n 1)))))

(time
   (verify/unbound #:guarantee (assert (= (fib n) (fib-swap n)))))