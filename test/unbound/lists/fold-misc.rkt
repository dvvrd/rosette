#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

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

(define fold-misc-tests
  (test-suite+
   "[unbound] Tests for lists/fold-misc.rkt"

   (check-unsat
     (verify/unbound (assert (<= rn mx)))
   )

   (check-sat
     (verify/unbound (assert (< rn mx)))
   )

   (check-unsat
     (verify/unbound (assert (<= mn rn)))
   )

   (check-sat
     (verify/unbound (assert (< mn rn)))
   )

   (check-unsat
     (verify/unbound (assert (<= mn mx)))
   )

   (check-sat
     (verify/unbound (assert (< mn mx)))
   )

   (check-unsat
     (verify/unbound (assert (implies (= mn mx) (= rn mx))))
   )

   (check-unsat
     (verify/unbound (assert (implies (= mn mx) (= rn mn))))
   )

   (check-unsat
     (verify/unbound (assert (implies (= mn mx) (= fs mn))))
   )

   (check-unsat
     (verify/unbound (assert (implies (= mn mx) (= fs mx))))
   )

   (check-unsat
     (verify/unbound (assert (implies (= mn mx) (= fs rn))))
   )

   (check-sat
     (verify/unbound (assert (implies (= mn mx) (< fs rn))))
   )
      
   (check-sat
     (verify/unbound (assert (implies (= rn mx) (= rn mn))))
   )

   (check-sat
     (verify/unbound (assert (implies (= rn mn) (= rn mx))))
   )

   (check-sat
     (verify/unbound (assert (implies (= mn fs) (> rn mn))))
   )
))

(time (run-tests fold-misc-tests))
