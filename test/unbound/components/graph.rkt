#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         "../../../rosette/base/unbound/graph.rkt")

; g1 is something like this:
;
; 1 4  6 8
; --------
; |\/\/\/|
; |/\/\/\|
; --------
; 2 3  5 7

(define g1 (make-graph))
(connect! g1 1 2) (connect! g1 1 3) (connect! g1 1 4)
(connect! g1 2 1) (connect! g1 2 3) (connect! g1 2 4)
(connect! g1 3 1) (connect! g1 3 2) (connect! g1 3 4) (connect! g1 3 5) (connect! g1 3 6)
(connect! g1 4 1) (connect! g1 4 2) (connect! g1 4 3) (connect! g1 4 5) (connect! g1 4 6)
(connect! g1 5 3) (connect! g1 5 4) (connect! g1 5 6) (connect! g1 5 7) (connect! g1 5 8)
(connect! g1 6 3) (connect! g1 6 4) (connect! g1 6 5) (connect! g1 6 7) (connect! g1 6 8)
(connect! g1 7 5) (connect! g1 7 6) (connect! g1 7 8)
(connect! g1 8 5) (connect! g1 8 6) (connect! g1 8 7)

(define g2 (make-graph))
(connect! g2 1 2) (connect! g2 2 1)

(define cliques (mutable-set))

(define (add-clique clique)
  (set-add! cliques
            (sort
             (set->list clique)
             <)))

(define graph-tests
  (test-suite+
   "[unbound] Tests for components/graph.rkt"

   (check-true
    (and (enumerate-cliques g1 add-clique)
         (= (set-count cliques) 3)
         (set-member? cliques '(1 2 3 4))
         (set-member? cliques '(3 4 5 6))
         (set-member? cliques '(5 6 7 8))))

   (check-true
    (and (set-clear! cliques)
         (enumerate-cliques g2 add-clique)
         (= (set-count cliques) 1)
         (set-member? cliques '(1 2))))))

(time (run-tests graph-tests))
