#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (c q) (~> integer? boolean?) #t)
(define/unbound b (x) (q) (~> integer? boolean?) (x 1))
(define/unbound a (x y) (q) (~> integer? boolean?)
  (if (zero? q)
      (begin
        (x 0)
        (y 0))
      (assert #f)))
(define/unbound f (x) (n q) (~> integer? integer? boolean?)
  (cond [(<= n 0) (x q)]
        [else (a x (λ (q) (f (λ (q) (b x q)) (- n 1) q)) q)]))
(define (s n q) (f c n q))

; Expecting unsat
(time
 (verify/unbound (assert (s n 0))))
