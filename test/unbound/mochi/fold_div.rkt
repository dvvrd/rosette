#lang rosette/unbound

(dbg-level 0)

(define-symbolic xs (listof integer?))
(define-symbolic m integer?)

(define/typed (positive? x) (~> integer? boolean?)
  (> x 0))
(define/typed (quotient x y) (~> integer? integer? integer?)
  (define-symbolic r integer?)
  (assert (not (zero? x)))
  (if (and (>= y 0) (> x 0)) (abs r) r))

; Expecting unsat
(verify/unbound
 (assert
  (=> (and (> m 0) (andmap positive? xs))
      (>= (foldl quotient m xs) 0))))

; Expecting sat
(verify/unbound
 (assert
  (=> (and (> m 0) (andmap positive? xs))
      (> (foldl quotient m xs) 0))))
