#lang rosette/unbound

(dbg-level 0)

(define-symbolic n integer?)

(define/unbound (lock st) (~> integer? integer?)
  (assert (= st 0)) 1)
(define/unbound (unlock st) (~> integer? integer?)
  (assert (= st 1)) 0)
(define/unbound (f n st)  (~> integer? integer? integer?)
  (if (> n 0) (lock st) st))
(define/unbound (g n st) (~> integer? integer? integer?)
  (if (>= n 0) (unlock st) st))

; Expecting sat
(verify/unbound (assert (zero? (g n (f n 0)))))

; Expecting unsat
(verify/unbound #:assume (assert (> n 0))
                #:guarantee (assert (zero? (g n (f n 0)))))
