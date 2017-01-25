#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n m integer?)

(define/unbound (fib n) (~> integer? integer?)
  (if (< n 2) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(time
   (verify/unbound #:assume (assert (< m n))
                   #:guarantee (assert (<= (fib m) (fib n)))))