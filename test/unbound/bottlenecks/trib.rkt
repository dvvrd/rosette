#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(ite-compactification #t)
(merge-accuracy 10)
(define-symbolic n m integer?)

(define/unbound (trib n) (~> integer? integer?)
  (if (< n 3) 1
      (+ (trib (- n 1))
         (trib (- n 2))
         (trib (- n 3)))))

(define/unbound (trib1 n) (~> integer? integer?)
  (if (< n 3) 1
      (+ (trib1 (- n 1))
         (trib1 (- n 2))
         (trib1 (- n 3)))))

(time
   (verify/unbound #:assume (assert (< m n))
                   #:guarantee (assert (<= (trib m) (trib n)))))