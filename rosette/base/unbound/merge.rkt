#lang racket

; This module performs merging of Horn clauses for hardly solvable clauses.
; Demonstrating it below on the example with clauses for folds.
;
; It tries to solve the comparison of two folds of the same list in query.
; For example if we have query (< (fold + 0 xs) (fold + 0 (map add1 xs))) we have such
; clauses:
;
; (l = 0) => (fold1 acc xs acc)
; (l > 0) & (fold1 (+ acc h )  xs' eps) => (fold1 acc xs eps)
; and
; (l = 0) => (fold2 acc xs acc)
; (l > 0) & (fold2 (+ acc h 1) xs' eps) => (fold2 acc xs eps)
;
; The problem that when we instantiate fold1 and fold2 we get two universally quantified
; 'h' constants (so solver thinks that those are two different non-deterministic values).
; The only way to solve this problem is to generate 'product' fold clause that will
; describe synchronious folds calculation:
;
; (l = 0) => (fold acc1 acc2 xs acc1 acc2)
; (l > 0) & (fold (+ acc1 h) (+ acc2 h 1) xs' eps1 eps2) => (fold acc1 acc2 xs eps1 eps2)
;
; The merging process is performed via merge/folds procedure. It accepts a system of arbitrary
; Horn clauses, detects fold patterns, merges clauses defining them and replaces applications
; of merged folds with applications of product fold.
;
; Note that sometimes we can't merge folds depending one on other, for example such ones:
; (fold + (fold - 0 xs) xs). The system automaticly builds graph of fold dependencies and
; merges only independent folds.

(require
  "horn.rkt"
  (only-in "../core/safe.rkt" assert)
  (only-in "../core/term.rkt" constant constant? expression type-of @app)
  (only-in "auto-constants.rkt" auto-premises term->constants/with-auto-premises)
  (only-in "relation.rkt" fresh-relation relation?)
  (only-in "utils.rkt" for**/list gensym substitute/constants))

(struct clauses-merger ()
  #:methods gen:horn-transformer
  [(define (pre-process self clauses)
     (build-dependencies clauses)
     (get-transitive-closure! dependencies-graph)
     (clear-caches))
   (define (post-process self terms) terms)])

(register-horn-transformer (clauses-merger))

;; ----------------- Dependencies ----------------- ;;

(define dependencies-graph (make-hash))
(define input-constants-cache (make-hash))
(define output-constants-cache (make-hash))

(define (arguments->input-output-constants f-app)
  (if (hash-has-key? input-constants-cache f-app)
      (values (hash-ref input-constants-cache f-app)
              (hash-ref output-constants-cache f-app))
      (match f-app
        [(expression (== @app) f f-args ...)
         (let*-values
             ([(input-terms output-constants)
               (splitf-at-right f-args
                                (λ (arg) (and (constant? arg)
                                              (member (auto-premises arg) f-app))))]
              [(input-constants)
               (apply set-union (cons (set)
                                      (map term->constants/with-auto-premises input-terms)))])
           (hash-set! input-constants-cache f-app input-constants)
           (hash-set! output-constants-cache f-app output-constants)
           (values input-constants output-constants))]
        [_ (values #f #f)])))

(define (depends?/definitions f g)
  (and (hash-has-key? dependencies-graph f)
       (set-member? (hash-ref dependencies-graph f) g)))

(define (independent?/definitions f g)
  (not (or (depends?/definitions f g)
           (depends?/definitions g f))))

(define (independent?/applications f-app g-app)
  (match* (f-app g-app)
    [((expression (== @app) f f-args ...) (expression (== @app) g g-args ...))
     (and (independent?/definitions f g)
          (let-values ([(f-input f-output) (arguments->input-output-constants f-app)]
                       [(g-input g-output) (arguments->input-output-constants g-app)])
            (and (set-empty? (set-intersect f-input g-output))
                 (set-empty? (set-intersect g-input f-output)))))]
    [(_ _) #f]))

(define (add-dependence f g)
  (set-add! (hash-ref! dependencies-graph f (thunk (mutable-set))) g))

(define (dfs/relation relation visited clauses)
  (define (dfs/term term)
    (match term
      [(expression _ args ...)
       (for ([arg args])
         (dfs/term arg))]
      [(constant _ _)
       (when (and (relation? term)
                  (not (equal? relation term))
                  (not (depends?/definitions relation term)))
         (printf "Definition of ~a depends on ~a\n" relation term)
         (add-dependence relation term)
         (dfs/relation term visited clauses))]
      [_ (void)]))

  (unless (set-member? visited relation)
    (set-add! visited relation)
    (for ([clause (hash-ref clauses relation '())])
      (for ([premise (in-set (horn-clause-premises clause))])
        (dfs/term premise)))))

; Builds graph of dependencies between relations.
(define (build-dependencies clauses)
  (let ([visited (mutable-set)])
    (for ([rel (in-hash-keys clauses)])
      (when rel
        (unless (set-member? visited rel)
          (dfs/relation rel visited clauses))))))

(define (get-transitive-closure! g)
  (define (dfs/closure v visited)
    (unless (set-member? visited v)
      (set-add! visited v)
      (for ([son (in-set (hash-ref! g v (thunk (mutable-set))))])
        (dfs/closure son visited)
        (set-union! (hash-ref g v) (hash-ref! g son (thunk (mutable-set)))))))
  (let ([visited (mutable-set)])
    (for ([v (in-hash-keys g)])
      (dfs/closure v visited))))

(define (clear-caches)
  (hash-clear! dependencies-graph)
  (hash-clear! input-constants-cache)
  (hash-clear! output-constants-cache))
