#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n m integer?)

(define/unbound (lucas n) (~> integer? integer?)
    (cond [(= n 1) 2]
          [(= n 2) 1]
          [else (+ (lucas (- n 1)) (lucas (- n 2)))]))

(define/unbound (lucas2 n) (~> integer? integer?)
    (cond [(= n 1) 2]
          [(= n 2) 1]
          [else (+ (lucas2 (- n 1)) (lucas2 (- n 2)))]))

(time
   (verify/unbound #:assume (assert (and (< 1 m) (< m n)))
                   #:guarantee (assert (< (lucas m) (lucas n)))))
