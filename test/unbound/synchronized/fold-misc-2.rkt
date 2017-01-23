#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)

(define-symbolic xs (listof integer?))

(define/typed (max/typed x y)
  (~> integer? integer? integer?)
  (max y x))

(define/typed (min/typed x y)
  (~> integer? integer? integer?)
  (min y x))

(define/typed (rnd/typed x y)
  (~> integer? integer? integer?)
  (if (random) y x))

(define fs (car xs))

(define mx (foldl max/typed 0 xs))
(define mn (foldl min/typed 0 xs))
(define rn (foldl rnd/typed 0 xs))

   (time
     (verify/unbound (assert (<= mn rn))))
