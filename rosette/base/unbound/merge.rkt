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
  "horn.rkt" "graph.rkt"
  (only-in "../../solver/solution.rkt" unsat?)
  (only-in "../../query/form.rkt" verify)
  (only-in "../core/bool.rkt" @! @&& ||)
  (only-in "../core/equality.rkt" @equal?)
  (only-in "../core/term.rkt" constant constant? expression type-of @app)
  (only-in "auto-constants.rkt" auto-premises term->constants/with-auto-premises)
  (only-in "relation.rkt" fresh-relation relation? decompose-arguments)
  (only-in "utils.rkt" for**/list gensym substitute/constants))

(struct clauses-merger ()
  #:methods gen:horn-transformer
  [(define (pre-process self clauses)
     (build-dependencies clauses)
     (transitive-close! dependencies-graph)
     (merge/clauses clauses)
     (clear-caches!))
   (define (post-process self terms) terms)])

(register-horn-transformer (clauses-merger))


(define dependencies-graph (make-graph))
(define input-constants-cache (make-hash))
(define output-constants-cache (make-hash))
(define matchings-cache (make-hash))
(define synchronized-cache (make-hash))
(define app-product-cache (make-hash))

(define (clear-caches!)
  (graph-clear! dependencies-graph)
  (hash-clear! input-constants-cache)
  (hash-clear! output-constants-cache)
  (hash-clear! matchings-cache)
  (hash-clear! synchronized-cache)
  (hash-clear! app-product-cache))

;; ----------------- Dependencies ----------------- ;;

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
  (connected? dependencies-graph f g))

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
  (connect! dependencies-graph f g))

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

;; ----------------- Determining synchronized set ----------------- ;;

; Remark: the optimal matching may be obtained by Kuhn algorithm.
; However, here we match recursive calls, so greedy algo works good
; for all practical cases.
(define (<-> id [xs #f] [ys #f] [ω #f])
  (hash-ref!
   matchings-cache id
   (and ω xs ys
    (thunk
     (let ([ys-to-match (list->mutable-set ys)]
           [matching (mutable-set)])
       (and
        (for/and ([x xs])
          (for/or ([y ys])
            (and (ω x y)
                 (set-add! matching (cons x y))
                 (set-remove! ys-to-match y))))
        (for/and ([y (in-set ys-to-match)])
          (for/or ([x (in-set xs)])
            (and
             (ω x y)
             (set-add! matching (cons x y)))))
        matching))))))

(define (rel-and-args-of application)
  (match application
    [(expression (== @app) rel args ...)
     (values rel args)]
    [_ (values #f #f)]))

(define (args-of application)
  (let-values ([(_ args) (rel-and-args-of application)])
    args))

(define (rel-of application)
  (let-values ([(rel _) (rel-and-args-of application)])
    rel))

(define (synchronized-by?/solve f-interpreted-premises g-interpreted-premises f-conclusion-args g-conclusion-args f-args g-args)
  (let ([premises (map @! `(,@(set->list f-interpreted-premises)
                            ,@(set->list g-interpreted-premises)
                            (for/list ([f-arg f-args]
                                       [g-arg g-args])
                              (@equal? (list-ref f-conclusion-args f-arg)
                                       (list-ref g-conclusion-args g-arg)))))])
    (λ (f-recursive-premise g-recursive-premise)
      (let* ([f-app-args (args-of f-recursive-premise)]
             [g-app-args (args-of g-recursive-premise)]
             [conclusion (apply @&& (for/list ([f-arg f-args]
                                               [g-arg g-args])
                                      (@equal? (list-ref f-app-args f-arg)
                                               (list-ref g-app-args g-arg))))])
        (unsat?
         (verify (apply || (cons conclusion premises))))))))

(define (split-premises premises f)
  (for/fold ([interpreted '()]
             [linear '()]
             [recursive '()])
            ([premise (in-set premises)])
    (let ([rel (rel-of premise)])
      (cond
        [(false? rel)   (values (cons premise interpreted) linear recursive)]
        [(equal? rel f) (values interpreted linear (cons premise recursive))]
        [else           (values interpreted (cons premise linear) recursive)]))))

(define (synchronized-by?/clauses f g f-id g-id f-args g-args)
  (let*-values
      ([(f-rel f-concl-args) (horn-clause-conclusion f)]
       [(g-rel g-concl-args) (horn-clause-conclusion g)]
       [(φ f-linear f-recursive) (split-premises (horn-clause-premises f) f-rel)]
       [(ψ g-linear g-recursive) (split-premises (horn-clause-premises g) g-rel)]
       [(ω) (synchronized-by?/solve φ ψ f-concl-args g-concl-args f-args g-args)])
    (<-> (list f-rel g-rel f-id g-id f-args g-args)
         (cons (horn-clause-conclusion f) f-recursive)
         (cons (horn-clause-conclusion g) g-recursive)
         ω)))

(define (synchronized-by?/definitions f g f-args g-args clauses)
  (hash-ref!
   synchronized-cache (list f g f-args g-args)
   (thunk
    (or (and (empty? f-args) (empty? g-args))
        (and (= (length f-args) (length g-args))
             (independent?/definitions f g)
             (for*/and ([(f-clause f-id) (in-sequences (hash-ref clauses f) (in-naturals))]
                        [(g-clause g-id) (in-sequences (hash-ref clauses g) (in-naturals))])
               (synchronized-by?/clauses f-clause g-clause f-id g-id f-args g-args)))))))

;; ----------------- Merging ----------------- ;;

(define (synchronous-product f g f-args g-args f-apps g-apps clauses)
  (define product-app
    (hash-ref!
     app-product-cache (list f g f-args g-args)
     (thunk
      (let*-values
          ([(f-read-deps f-args f-write-deps f-rets) (decompose-arguments f)]
           [(g-read-deps g-args g-write-deps g-rets) (decompose-arguments g)]
           [(h) (fresh-relation (gensym (format "~a⊕~a" f g))
                                (append f-read-deps g-read-deps)
                                (append f-args g-args)
                                (append f-write-deps g-write-deps)
                                (append f-rets g-rets))]
           [(product-app)
            (λ (f-app g-app)
              (let*-values
                  ([(f-read-deps f-args f-write-deps f-rets) (decompose-arguments f-app)]
                   [(g-read-deps g-args g-write-deps g-rets) (decompose-arguments g-app)])
                (apply expression `(,h ,@f-read-deps ,@g-read-deps ,@f-args ,@g-args ,@f-write-deps ,@g-write-deps ,@f-rets ,@g-rets))))]
           [(h-defs)
            (for*/list ([(f-clause f-id) (in-sequences (hash-ref clauses f) (in-naturals))]
                        [(g-clause g-id) (in-sequences (hash-ref clauses g) (in-naturals))])
              (let*-values
                  ([(f-conclusion) (horn-clause-conclusion f-clause)]
                   [(g-conclusion) (horn-clause-conclusion g-clause)]
                   [(h-conclusion) (product-app f-conclusion g-conclusion)]
                   [(f-conclusion-args) (args-of f-conclusion)]
                   [(g-conclusion-args) (args-of g-conclusion)]
                   [(equalities) (for/list ([f-arg f-args]
                                            [g-arg g-args])
                                   (@equal? (list-ref f-conclusion-args f-arg)
                                            (list-ref g-conclusion-args g-arg)))]
                   [(φ f-linear f-recursive) (split-premises (horn-clause-premises f-clause) f)]
                   [(ψ g-linear g-recursive) (split-premises (horn-clause-premises g-clause) g)]
                   [(matching) (<-> (list f g f-id g-id f-args g-args))]
                   [(h-recursive)
                    (filter (negate (curry equal? h-conclusion))
                            (map (curry apply product-app) matching))]
                   [(h-premises) (append equalities φ ψ f-linear g-linear h-recursive)])
                (horn-clause (list->set h-premises) h-conclusion)))])
        (hash-set! clauses h h-defs)
        product-app))))
  (for/list ([f-app f-apps]
             [g-app g-apps])
    (product-app f-app g-app)))

(define (merge/linear-premises premises clauses)
  (define graphs (make-hash))
  (define (for*/one lst proc)
    (unless (empty? lst)
      (let ([h (car lst)]
            [t (cdr lst)])
        (for ([e t])
          (proc h e)))
      (for*/one t proc)))

  ; Episode 1: building graphs. For each set of synchronized arguments we
  ; connect two applications iff they synchronized by this set. So we get the
  ; same amount of graphs as there are synchronized sets of arguments.

  ; Episode 2: for each graph calculating all its non-trivial cliques.

  ; Episode 3.1: selecting best clique-partitioning of the set of calls.
  ; For that building graph of cliques: two cliques are conected iff they
  ; have no common vertices. Each clique has its weight meaning the
  ; decreasing of 'entropy', or the how much information will be brought to
  ; solver if clauses for this clique will be merged into one set of clauses.

  ; Episode 3.2: finally the best partitioning is the biggest-weight subclique
  ; in the graph of cliques :)
  )

(define (merge/clauses clauses)
  (for ([rel (in-hash-keys clauses)])
    (for ([clause (hash-ref clauses rel)])
      (let-values ([(φ linear recursive) (split-premises (horn-clause-premises clause rel))])
        (merge/linear-premises linear clauses)))))
