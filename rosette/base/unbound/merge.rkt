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
  "horn.rkt" "graph.rkt" "utils.rkt"
  (only-in "../../solver/solution.rkt" unsat?)
  (only-in "../../query/form.rkt" verify)
  (only-in "../core/bool.rkt" @! @&& ||)
  (only-in "../core/equality.rkt" @equal?)
  (only-in "../core/safe.rkt" assert)
  (only-in "../core/term.rkt" constant constant? expression type-of @app term<?)
  (only-in "auto-constants.rkt" auto-premises term->constants/with-auto-premises)
  (only-in "dependencies.rkt" implicit-dependencies)
  (only-in "relation.rkt" fresh-relation relation? decompose-arguments relation-suffix))

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
                                              (set-member? (auto-premises arg) f-app))))]
              [(output-constants) (list->set output-constants)]
              [(input-constants)
               (apply set-union (cons (set)
                                      (map term->constants/with-auto-premises input-terms)))])
           (hash-set! input-constants-cache f-app input-constants)
           (hash-set! output-constants-cache f-app output-constants)
           (values  input-constants output-constants))]
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

(define (<-> id [xs #f] [ys #f] [ω #f])
  (hash-ref!
   matchings-cache id
   (and ω xs ys
    (thunk
     (let ([xs-to-match (list->mutable-set xs)]
           [ys-to-match (list->mutable-set ys)]
           [matching (mutable-set)])
       (for ([x xs])
         (for ([y ys])
           (when (ω x y)
             (set-add! matching (list x y))
             (set-remove! xs-to-match x)
             (set-remove! ys-to-match y))))
       (and
        (set-empty? xs-to-match)
        (set-empty? ys-to-match)
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
  (let ([premises
         (map @!
              (append (set->list f-interpreted-premises)
                      (set->list g-interpreted-premises)
                      (apply append
                             (for/list ([f-arg f-args]
                                        [g-arg g-args])
                               (let* ([f-arg-val (list-ref f-conclusion-args f-arg)]
                                      [g-arg-val (list-ref g-conclusion-args g-arg)]
                                      [f-arg-deps (implicit-dependencies f-arg-val)]
                                      [g-arg-deps (implicit-dependencies g-arg-val)])
                                 (cons (@equal? f-arg-val g-arg-val)
                                       (for/list ([f-dep f-arg-deps]
                                                  [g-dep g-arg-deps])
                                         (@equal? f-dep g-dep))))))))])
    (λ (f-recursive-premise g-recursive-premise)
      (let* ([f-app-args (args-of f-recursive-premise)]
             [g-app-args (args-of g-recursive-premise)]
             [conclusion (apply @&& (for/list ([f-arg f-args]
                                               [g-arg g-args])
                                      (@equal? (list-ref f-app-args f-arg)
                                               (list-ref g-app-args g-arg))))])
        (with-handlers ([exn:fail? (λ (err) #f)])
          (unsat?
           (verify (assert (apply || (cons conclusion premises))))))))))

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
      ([(f-rel f-concl-args) (rel-and-args-of (horn-clause-conclusion f))]
       [(g-rel g-concl-args) (rel-and-args-of (horn-clause-conclusion g))]
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
    (and (= (length f-args) (length g-args))
         (independent?/definitions f g)
         (or (and (empty? f-args) (empty? g-args))
             (for/and ([f-clause (hash-ref clauses f)]
                       [f-id (in-naturals)]
                       #:when #t
                       [g-clause (hash-ref clauses g)]
                       [g-id (in-naturals)])
               (synchronized-by?/clauses f-clause g-clause f-id g-id f-args g-args)))))))

;; ----------------- Merging ----------------- ;;

(define (synchronous-product arg-nums apps clauses)
  (define sorted-apps (sort apps term<? #:key rel-of))
  (define sorted-rels (map rel-of sorted-apps))
  (define sorted-idxs (map index-of sorted-apps))
  (define sorted-args
    (for*/list ([(f rest-idxs) (in-splits sorted-idxs)]
                [g rest-idxs])
      (arg-nums f g)))
  (define key (append sorted-rels sorted-args))

  (define (match/cliques clauses-nums recursive-premises)
    (define n (length clauses-nums))
    (define sync-graph (make-graph))
    (define cliques (make-hash))
    (define indeces (mutable-set))

    (and
     (for/and ([(f rest-rels) (in-splits sorted-rels)]
               [(f-idx rest-idxs) (in-splits sorted-idxs)]
               [(f-clause-num rest-clause-nums) (in-splits clauses-nums)]
               [(f-premises rest-premises) (in-splits recursive-premises)]
               #:when #t
               [g rest-rels]
               [g-idx rest-idxs]
               [g-clause-num rest-clause-nums]
               [g-premises rest-premises])
       (let*-values
           ([(f-arg-nums g-arg-nums) (unzip (arg-nums f-idx g-idx))]
            [(matching)
             (if (empty? f-arg-nums)
                 (cartesian-product f-premises g-premises)
                 (<-> (list f g f-clause-num g-clause-num f-arg-nums g-arg-nums)))])
         (and
          matching
          (for ([pair matching])
            (let ([f-app-idx (index-of (cons (first pair) f-idx))]
                  [g-app-idx (index-of (cons (last pair) g-idx))])
              (set-add! indeces f-app-idx)
              (set-add! indeces g-app-idx)
              (connect!/undirected sync-graph f-app-idx g-app-idx))))))

     (enumerate-cliques sync-graph
      (λ (clique)
        (when (= (set-count clique) n)
          (let ([copied-clique (list->set (set->list clique))])
            (for ([v (in-set copied-clique)]
                  #:unless (hash-has-key? cliques v))
              (hash-set! cliques v copied-clique))))))

     (= (hash-count cliques) (set-count indeces))

     (list->set
      (map (λ (clique)
             (set-map clique (compose car vertex-of)))
           (hash-values cliques)))))

  (define product-app
    (hash-ref!
     app-product-cache key
     (thunk
      (let*-values
          ([(read-deps args write-deps rets)
            (for/lists (l1 l2 l3 l4) ([rel sorted-rels])
              (decompose-arguments rel rel))]
           [(h) (fresh-relation (gensym (string-join
                                         (map
                                          (λ (rel)
                                            (string-trim (~a rel) relation-suffix))
                                          sorted-rels)
                                         "⊕"))
                                (apply append read-deps)
                                (apply append args)
                                (apply append write-deps)
                                (apply append rets))]
           [(product-app)
            (λ (apps)
              (let*-values
                  ([(sorted-apps) (sort apps term<? #:key rel-of)]
                   [(read-deps args write-deps rets)
                    (for/lists (l1 l2 l3 l4) ([app sorted-apps])
                      (decompose-arguments (rel-of app) app))])
                (apply expression `(, @app ,h
                                           ,@(apply append read-deps)
                                           ,@(apply append args)
                                           ,@(apply append write-deps)
                                           ,@(apply append rets)))))]
           [(h-defs)
            (for**/list
             (map (compose range length (curry hash-ref clauses)) sorted-rels)
             (λ (clauses-nums)
               (let*-values
                   ([(cur-clauses)
                     (for/list ([rel sorted-rels]
                                [num clauses-nums])
                       (list-ref (hash-ref clauses rel) num))]
                    [(conclusion-args) (map (compose args-of horn-clause-conclusion) cur-clauses)]
                    [(h-conclusion) (product-app (map horn-clause-conclusion cur-clauses))]
                    [(equalities)
                     (apply append
                      (for/list ([(f-idx rest-idxs) (in-splits sorted-idxs)]
                                 [(f-conclusion-args rest-conclusions) (in-splits conclusion-args)]
                                 #:when #t
                                 [g-idx rest-idxs]
                                 [g-conclusion-args rest-conclusions]
                                 #:when #t
                                 [pair (arg-nums f-idx g-idx)])
                        (let* ([f-arg (car pair)]
                               [g-arg (cdr pair)]
                               [f-arg-val (list-ref f-conclusion-args f-arg)]
                               [g-arg-val (list-ref g-conclusion-args g-arg)]
                               [f-arg-deps (implicit-dependencies f-arg-val)]
                               [g-arg-deps (implicit-dependencies g-arg-val)])
                          (cons (@equal? f-arg-val g-arg-val)
                                (for/list ([f-dep f-arg-deps]
                                           [g-dep g-arg-deps])
                                  (@equal? f-dep g-dep))))))]
                    [(clauses) (map (λ (f clause-num) (list-ref (hash-ref clauses f) clause-num)) sorted-rels clauses-nums)]
                    [(φs linears recursives)
                     (for/lists (l1 l2 l3) ([rel sorted-rels]
                                            [clause clauses])
                       (split-premises (horn-clause-premises clause) rel))]
                    [(matching) (match/cliques clauses-nums
                                               (map (λ (apps clause)
                                                      (cons (horn-clause-conclusion clause) apps))
                                                    recursives clauses))])
                 (assert matching)
                 (let* ([h-recursive
                         (filter (negate (curry equal? h-conclusion))
                                 (set-map matching product-app))]
                        [h-premises (append equalities (apply append φs) (apply append linears) h-recursive)])
                   (horn-clause (list->set h-premises) h-conclusion)))))])
        (hash-set! clauses h h-defs)
        product-app))))

  (and product-app
       (product-app apps)))

(define vertices-of-indices (make-hash))
(define indices-of-vertices (make-hash))

(define (index-of vertex)
  (hash-ref! indices-of-vertices vertex
             (thunk
              (let ([idx (add1 (hash-count indices-of-vertices))])
                (hash-set! vertices-of-indices idx vertex)
                idx))))

(define (vertex-of index)
  (hash-ref vertices-of-indices index))

(define (merge/linear-premises premises clauses)
  (define graph (make-graph))
  (define sync-sets (make-hash))

  (define (synchronize f g s)
    (hash-set! sync-sets (cons f g) s)
    (hash-set! sync-sets (cons g f) s)
    (connect!/undirected graph f g))

  ; Episode 1: building graph. For each set of synchronized arguments we
  ; connect two applications iff they synchronized by this set. So we get the
  ; same amount of graphs as there are synchronized sets of arguments.
  (for* ([(f-app rest-premises) (in-splits premises)]
         [g-app rest-premises])
    (when (independent?/applications f-app g-app)
      (let ([f-ind (index-of f-app)]
            [g-ind (index-of g-app)])
        (synchronize f-ind g-ind '())
        (let*-values
            ([(f-rel f-args*) (rel-and-args-of f-app)]
             [(g-rel g-args*) (rel-and-args-of g-app)]
             [(f-app f-rel f-args* g-app g-rel g-args*)
              (if (term<? f-rel g-rel)
                  (values f-app f-rel f-args* g-app g-rel g-args*)
                  (values g-app g-rel g-args* f-app f-rel f-args*))]
             [(f-read-deps f-args f-write-deps f-ret) (decompose-arguments f-rel f-args*)]
             [(g-read-deps g-args g-write-deps g-ret) (decompose-arguments g-rel g-args*)]
             [(initial-synchronizations)
              (for*/list ([f-arg (in-range (length f-read-deps)
                                           (+ (length f-read-deps) (length f-args)))]
                          [g-arg (in-range (length g-read-deps)
                                           (+ (length g-read-deps) (length g-args)))]
                          #:when (and (equal? (list-ref f-args f-arg)
                                              (list-ref g-args g-arg))
                                      (synchronized-by?/definitions f-rel g-rel
                                                                    (list f-arg) (list g-arg)
                                                                    clauses)))
                (synchronize f-ind g-ind (list (cons f-arg g-arg)))
                (cons f-arg g-arg))])
          (for ([synchronization (in-combinations initial-synchronizations)]
                #:when (and (>= (length synchronization) 2)
                            (let-values ([(f-args g-args) (unzip synchronization)])
                              (synchronized-by?/definitions f-rel g-rel
                                                            f-args g-args
                                                            clauses))))
            (synchronize f-ind g-ind synchronization))))))

  ; Episode 2: for each graph enumerating all its maximal by inclusion sub-cliques.
  (define cliques-graph (make-graph))
  (define weights-of-cliques (make-hash))
  (define max-weight 0)
  (define max-clique #f)

  (define (weight-of clique)
    (+ (set-count clique)
       (for*/sum ([(v1 rest-vs) (in-splits (set->list clique))]
                  [v2 rest-vs])
         (length (hash-ref sync-sets (cons v1 v2))))))

  (enumerate-cliques graph
   (λ (clique)
     (let* ([copied-clique (list->set (set->list clique))]
            [idx (index-of copied-clique)]
            [weight (weight-of clique)])
       (when (> weight max-weight)
         (set! max-weight weight)
         (set! max-clique (set idx)))
       (hash-set! weights-of-cliques idx weight))))

  ; Episode 3.1: selecting best clique-partitioning of the set of calls.
  ; For that building graph of cliques: two cliques are conected iff they
  ; have no common vertices. Each clique has its weight meaning the
  ; decreasing of 'entropy', or the how much information will be brought to
  ; solver if clauses for this clique will be merged into one set of clauses.
  (for* ([(idx1 rest-cliques) (in-splits (hash-keys weights-of-cliques))]
         [idx2 rest-cliques])
    (when (set-empty?
           (set-intersect (vertex-of idx1)
                          (vertex-of idx2)))
      (connect!/undirected cliques-graph idx1 idx2)))

  ; Episode 3.2: the best partitioning is the biggest-weight subclique
  ; in the graph of cliques.
  (enumerate-cliques cliques-graph
   (λ (clique-of-cliques)
     (define weight
       (for/sum ([c (in-set clique-of-cliques)])
         (hash-ref weights-of-cliques c)))
     (when (> weight max-weight)
       (set! max-weight weight)
       (set! max-clique (set-copy clique-of-cliques)))))

  ; Episode 4: finally performing merging of largest possible amount of premises.
  (if max-clique
      (for/fold ([premises (list->set premises)])
                ([idx (in-set max-clique)])
        (let* ([clique (vertex-of idx)]
               [apps (set-map clique vertex-of)]
               [product (synchronous-product (λ (f g) (hash-ref sync-sets (cons f g)))
                                             apps clauses)]
               [product
                (or product
                    (begin
                      (displayln (string-append "Note: your case is considered by the system as very interesting! "
                                                "Please share it with developers in order to improve this tool!"))
                      (synchronous-product (λ (f g) (list)) apps clauses)))])
          (set-union (set product) (set-subtract premises (list->set apps)))))
      (list->set premises)))

(define (merge/clauses clauses)
  (define visited (mutable-set #f))
  (define queue (list #f))
  (define (enqueue rel)
    (unless (set-member? visited rel)
      (set-add! visited rel)
      (set! queue (append queue (list rel)))))
  (define (dequeue)
    (begin0
      (first queue)
      (set! queue (rest queue))))

  (let loop ()
    (unless (empty? queue)
      (define rel (dequeue))
      (define new-clauses
        (for/list ([clause (hash-ref clauses rel '())])
          (let*-values
              ([(φ linear recursive) (split-premises (horn-clause-premises clause) rel)]
               [(merged-linear-premises) (merge/linear-premises linear clauses)])
            (for ([premise (in-set merged-linear-premises)])
              (enqueue (rel-of premise)))
            (horn-clause (set-union (list->set φ) merged-linear-premises (list->set recursive))
                         (horn-clause-conclusion clause)))))
      (hash-set! clauses rel new-clauses)
      (loop))))