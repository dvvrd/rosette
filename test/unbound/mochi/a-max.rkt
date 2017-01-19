#lang rosette/unbound

(dbg-level 1)
(define-symbolic n i integer?)

(define (make-array n i)
  (- n i))

(define/unbound array-max (a) (n i m) (~> integer? integer? integer? integer?)
  (cond [(>= i n) m]
        [else
         (let* ([x (a i)]
                [z (if (> x m) x m)])
           (array-max a n (+ i 1) z))]))

; Expecting unsat
(verify/unbound
 #:assume (assert (and (> n 0) (>= i 0) (<= i 0)))
 #:guarantee
 (let ([m (array-max (curry make-array n) n i -1)])
   (assert (>= m n))))
