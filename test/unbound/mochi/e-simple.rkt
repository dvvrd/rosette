#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (f n k)
  (cond [(>= n 0) (void)]
        [else (k 0)]))

(define (g n)
  (assert (zero? n)))

; Expecting unsat
(time
 (verify/unbound (assert (and (f n g) #t))))
