#lang rosette/unbound

(dbg-level 0)

(define-symbolic xs (listof integer?))
(define-symbolic m integer?)

(define/typed (non-negative? x) (~> integer? boolean?)
  (>= x 0))
(define/typed (quotient x y) (~> integer? integer? integer?)
  (define-symbolic r integer?)
  (assert (not (zero? x)))
  (if (and (>= y 0) (> x 0)) (abs r) r))

; Expecting sat
(verify/unbound #:assume (assert (and (> m 0) (andmap non-negative? xs)))
                #:guarantee (assert (>= (foldl quotient m xs) 0)))
