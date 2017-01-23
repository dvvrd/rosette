#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(ite-compactification #f)
(define-symbolic n integer?)

(define/unbound (fib-shift n) (~> integer? integer?)
  (if (< n 2) 1
      (+ (fib-shift (- n 1))
         (fib-shift (- n 2)))))

(define/unbound (fib n) (~> integer? integer?)
  (if (< n 2) n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(time
   (verify/unbound #:assume (assert (> n 1))
                   #:guarantee (assert (= (fib n) (fib-shift (- n 1))))))