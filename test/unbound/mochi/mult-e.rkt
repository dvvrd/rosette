#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (mult n m) (~> integer? integer? integer?)
  (cond [(or (<= n 0) (<= m 0)) 0]
        [else (+ n (mult n (- m 1)))]))

; Expecting sat
(time
 (verify/unbound (assert (<= (+ n 1) (mult n n)))))
