#lang rosette/unbound

(dbg-level 0)
(define-symbolic x n m integer?)

(define/unbound exists (test f) (n m) (~> integer? integer? integer?)
  (if (< n m)
      (if (test (f n))
          n
          (exists test f (add1 n) m))
      -1))

(define (mult3 n) (* 3 n))

(define (test x) (= x m))
(define e (exists test mult3 0 n))

; Expecting unsat
(verify/unbound #:assume (assert (>= e 0))
                #:guarantee (assert (and (<= 0 e) (< e n))))
; Expecting sat
(verify/unbound #:assume (assert (>= e 0))
                #:guarantee (assert (and (<= 0 e) (>= e n))))
