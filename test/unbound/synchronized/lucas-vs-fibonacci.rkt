#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (lucas n) (~> integer? integer?)
    (cond [(= n 1) 2]
          [(= n 2) 1]
          [else (+ (lucas (- n 1)) (lucas (- n 2)))]))

(define/unbound (fib n) (~> integer? integer?)
    (cond [(= n 1) 1]
          [(= n 2) 1]
          [else (+ (fib (- n 1)) (fib (- n 2)))]))

(time
   (verify/unbound #:assume (assert (> n 2))
                   #:guarantee (assert (< (fib n) (lucas n)))))
