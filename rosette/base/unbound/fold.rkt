#lang racket

; This module performs merging of Horn clauses for folds of lists.
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
  (only-in "../core/term.rkt" constant expression type-of)
  (only-in "auto-constants.rkt" auto-premises)
  (only-in "relation.rkt" fresh-relation)
  (only-in "utils.rkt" for**/list gensym substitute/constants))

(provide deferred-merge merge/folds)

(define folds-of-lists (make-hash))
(define lists-of-folds (make-hash))
(define folds-graph (make-hash))

(define (in-kids fold)
  (in-set (hash-ref! folds-graph fold (thunk (mutable-set)))))

(define (fold-of? fold-constant list-constant)
  (and (hash-has-key? lists-of-folds fold-constant)
       (set-member? (hash-ref lists-of-folds fold-constant) list-constant)))

(define (depends? fold1 fold2)
  (and (hash-has-key? folds-graph fold1)
       (set-member? (hash-ref folds-graph fold1) fold2)))

(define (add-folds-dependence fold1 fold2)
  (set-add! (hash-ref! folds-graph fold1 (thunk (mutable-set))) fold2))

(define (deferred-merge xs fold-constant)
  (set-add! (hash-ref! folds-of-lists xs (thunk (mutable-set))) fold-constant)
  (set-add! (hash-ref! lists-of-folds fold-constant (thunk (mutable-set))) xs))

(define (dfs/folds fold-constant list-constant visited)
  (define (dfs/folds/term term)
    (match term
      [(expression _ args ...)
       (for ([arg args])
         (dfs/folds/term arg))]
      [(constant _ _)
       (when (and (fold-of? term list-constant)
                  (not (equal? term fold-constant))
                  (not (depends? fold-constant term)))
         (printf "fold ~a depends on ~a\n" fold-constant term)
         (add-folds-dependence fold-constant term)
         (dfs/folds term list-constant visited))]
      [_ (void)]))

  (unless (set-member? visited fold-constant)
    (set-add! visited fold-constant)
    (let ([auto-premises (auto-premises fold-constant)])
      (for ([premise auto-premises])
        (dfs/folds/term premise)))))

; Searches dependencies between folds of a same list.
(define (build-folds-dependencies fold-constants list-constant)
  (let ([visited (mutable-set)])
    (for ([fold-constant fold-constants])
      (unless (set-member? visited fold-constant)
        (dfs/folds fold-constant list-constant visited)))))

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

(define (can-merge? fold1 fold2)
  (not (or (depends? fold1 fold2)
           (depends? fold2 fold1))))

; TODO: some cases still can't be verified with such greedy strategy.
; We should come up with more smart aproach.
(define (fold-merge-components/greedy folds)
  (group-by identity folds can-merge?))

(define-match-expander fold-expression
  (lambda (stx)
    (syntax-case stx ()
      [(_ id-pat args-pat ...) #'(expression app id-pat args-pat ...)])))

(define (fold? id args)
  (and (regexp-match-exact? #rx"fold.[0-9]+°" (~a id))
       (>= (length args) 3)))

(define (term->fold-id term)
  (match term
    [(fold-expression id args ...) #:when (fold? id args)
     id]
    [_ #f]))

(define (clause->fold-id clause)
  (term->fold-id (horn-clause-conclusion clause)))

(define (fold-base-clause? clause)
  (match (horn-clause-conclusion clause)
    [(fold-expression id args ...)
     (and (fold? id args)
          (equal? (~a (last args)) "init"))]
    [_ #f]))

(define (decompose-fold-params clause)
  (define (index-of xs proc)
    (for/or ([x xs] [i (in-naturals)] #:when (proc x)) i))
  (define (accumulator? arg)
    (equal? (~a arg) "init"))
  (define (folded-list? arg)
    (equal? (~a arg) "lst"))

  (match (horn-clause-conclusion clause)
    [(fold-expression id args ...)
     #:when (and (fold? id args) (accumulator? (last args)))
     (let* ([acc-position (index-of args accumulator?)]
            [lst-position (index-of args folded-list?)]
            [tail-length (- (length args) lst-position 1)])
       (and acc-position lst-position (= (add1 acc-position) lst-position)
            (λ (fold)
              (match fold
                [(fold-expression other-id other-args ...) #:when (fold? other-id other-args)
                 (assert (and (equal? id other-id) (= (length args) (length other-args)))
                         (thunk (error 'merge/folds "The application of a horn clause has ~a arguments, but the definition has ~a!"
                                   (length other-args) (length args))))
                 (values (take other-args acc-position)                                  ; Read-dependencies
                         (list-ref other-args acc-position)                              ; Accumulator arg
                         (list-ref other-args lst-position)                              ; List arg
                         (take (drop other-args (add1 lst-position)) (sub1 tail-length)) ; Write-dependencies
                         (last other-args))]))))]                                        ; Resulting constant
    [_ #f]))

(define (extract-folding-results clauses)
  (let ([result (make-hash)])
    (for ([clause clauses])
      (for ([premise (in-set (horn-clause-premises clause))])
        (match premise
          [(fold-expression id args ...) #:when (fold? id args)
           (hash-set! result (last args) (cons id premise))]
          [_ (void)])))
    result))

(define (group-by-folds clauses)
  (let ([groupped (group-by clause->fold-id clauses)])
    (for/hash ([g groupped] #:unless (empty? g))
      (values (clause->fold-id (car g)) g))))

(define (merge-one-group group fold-application-terms clauses lst fold-definitions folds-substitution)
  (define (mine-folds-args-and-bases! decomposers)
    (for/fold ([bases '()]
               [bodies '()]
               [product-read-dependencies '()]
               [product-accumulators '()]
               [product-lst #f]
               [product-write-dependencies '()]
               [product-results '()])
              ([f group])
         (let*-values ([(definitions) (hash-ref fold-definitions f '())]
                       [(base body) (partition fold-base-clause? definitions)]
                       [(base) (begin (assert (= 1 (length base))) (car base))]
                       [(decomposer) (decompose-fold-params base)]
                       [(read-deps acc xs write-deps result) (decomposer (horn-clause-conclusion base))]
                       [(new-acc) (constant (gensym (~a acc)) (type-of acc))])
           (hash-set! decomposers f decomposer)
           (values (cons base bases)
                   (cons body bodies)
                   (append read-deps product-read-dependencies)
                   (cons (cons acc new-acc) product-accumulators)
                   (begin
                     (and product-lst
                          (assert (equal? product-lst xs)
                                  (thunk (error 'merge/folds
                                                "Merging folds with different list arguments: expected ~a, got ~a!"
                                                product-lst xs))))
                     xs)
                   (append write-deps product-write-dependencies)
                   (cons new-acc product-results)))))

  (define (merge/fold-applications product-rel folds decomposers [accumulators #f])
    (define-values (read-depss accs lsts write-depss results)
      (for/lists (l1 l2 l3 l4 l5) ([f folds] [g group])
        ((hash-ref decomposers g) f)))
    (assert (and (not (empty? lsts)) (apply equal? lsts))
            (thunk (error 'merge/folds
                          "Merging folds with invalid list arguments: ~a!"
                          lsts)))
    (apply product-rel `(,@(apply append read-depss)
                         ,@(or accumulators accs)
                         ,(car lsts)
                         ,@(apply append write-depss)
                         ,@results)))
  
  (define (merge/fold-bodies pfold decomposers accumulators clauses)
    (assert (equal? group (map clause->fold-id clauses)))
    (let*-values
        ([(premises*) (map horn-clause-premises clauses)]
         [(conclusions) (map horn-clause-conclusion clauses)]
         [(folds premises)
          (for/lists (l1 l2) ([premises premises*]
                              [f group]
                              [acc-pair accumulators])
            (partition (λ (p) (equal? f (term->fold-id p)))
                       (set-map premises (curry substitute/constants (make-hash (list acc-pair))))))]
         [(premises-union) (foldl (λ (p acc) (set-union acc (list->set p))) (set) premises)]
         [(folds) (map (λ (f) (assert (= 1 (length f))) (car f)) folds)]
         [(resulting-premises)
          (set-add premises-union
                   (merge/fold-applications pfold folds decomposers))]
         [(resulting-conclusion) (merge/fold-applications pfold conclusions decomposers (map cdr accumulators))]
         [(product-application) (merge/fold-applications pfold fold-application-terms decomposers)])
      (for ([term fold-application-terms])
        (hash-set! folds-substitution term product-application))
      (horn-clause resulting-premises resulting-conclusion)))

  (match group
    [(list) clauses]
    [(list f) (append (hash-ref fold-definitions f (list)) clauses)]
    [_
     (let*-values
         ([(decomposers) (make-hash)]
          [(bases bodies read-deps accumulators lst-param write-deps results)
           (mine-folds-args-and-bases! decomposers)]
          [(bases accumulators results) (values (reverse bases) (reverse accumulators) (reverse results))]
          [(product-args) `(,@read-deps ,@(map cdr accumulators) ,lst-param ,@write-deps ,@results)]
          [(product-fold) (fresh-relation (gensym "⊕fold") (map type-of product-args))]
          [(new-base) (horn-clause (apply set-union (map horn-clause-premises bases))
                                   (apply product-fold product-args))]
          [(body-clauses) (for**/list (reverse bodies) (curry merge/fold-bodies product-fold decomposers accumulators))])
       `(,new-base ,@body-clauses ,@clauses))]))

(define (merge/folds clauses)
  (let* ([fold-clauses (group-by-folds clauses)]
         [clauses (hash-ref fold-clauses #f '())]
         [folding-results (extract-folding-results clauses)])
    (for/fold ([clauses clauses])
              ([lst (in-hash-keys folds-of-lists)])
      (let ([fold-constants (set->list (hash-ref folds-of-lists lst (thunk (set))))])
        (build-folds-dependencies fold-constants lst)
        (get-transitive-closure! folds-graph)
        (begin0
          (let* ([merge-sets (fold-merge-components/greedy fold-constants)]
                 [folds-substitution (make-hash)]
                 [_ (printf "merging sets for ~a: ~a\n" lst merge-sets)]
                 [merged-clauses
                  (for/fold ([clauses clauses])
                            ([group merge-sets])
                    (let*-values
                        ([(folds)
                          (filter identity
                                  (map
                                   (λ (const)
                                     (hash-ref folding-results const #f))
                                   group))]
                         [(ids terms) (for/lists (l1 l2) ([f folds]) (values (car f) (cdr f)))])
                      (merge-one-group ids terms clauses lst fold-clauses folds-substitution)))])
            (map (λ (clause)
                   (horn-clause
                    (list->set (set-map (horn-clause-premises clause) (λ (p) (hash-ref folds-substitution p p))))
                    (horn-clause-conclusion clause)))
                 merged-clauses))
          (hash-clear! folds-graph))))))
