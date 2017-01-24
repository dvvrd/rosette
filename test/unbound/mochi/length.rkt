#lang rosette/unbound

(dbg-level 0)
(define-symbolic xs (listof integer?))

(define/unbound (sum n) (~> integer? integer?)
  (cond [(<= n 0) 0]
        [else (+ n (sum (- n 1)))]))

(define/unbound (length/rec xs) (~> (listof integer?) integer?)
  (if (null? xs) 0 (add1 (length/rec (cdr xs)))))

; Expecting unsat
(time
 (verify/unbound (assert (= (length xs) (length/rec xs)))))
