#lang racket

(provide (all-defined-out))

; Creates new mutable oriented graph instance.
(define (make-graph) (make-hash))

; Returns #t iff graph g has oriented edge from v1 to v2 and #f otherwise.
(define (connected? g v1 v2)
  (and (hash-has-key? g v1)
       (set-member? (hash-ref g v1) v2)))

; Mutates g adding directed edge between v1 and v2.
(define (connect! g v1 v2)
  (set-add! (hash-ref! g v1 (thunk (mutable-set))) v2))

; Mutates g adding undirected edge between v1 and v2.
(define (connect!/undirected g v1 v2)
  (connect! g v1 v2)
  (connect! g v2 v1))

; Returns sequence of vertices connected to v in graph g.
(define (in-connected g v)
  (in-set (hash-ref g v (set))))

; Returns a list of vertices in graph g.
(define (vertices g)
  (hash-keys g))

; Returns a sequence of vertices in graph g.
(define (in-vertices g)
  (in-hash-keys g))

; Deep-first searches the graph g calling proc in each vertex (once).
(define (traverse/dfs g proc)
  (define (dfs v visited)
    (unless (set-member? visited v)
      (set-add! visited v)
      (proc v)
      (for ([w (in-connected g v)])
        (dfs w visited))))
  (let ([visited (mutable-set)])
    (for ([v (in-vertices g)])
      (dfs v visited))))

; Extends set of edges of g adding edges from v1 to v2 iff
; v2 is reachable from v1.
(define (transitive-close! g)
  (define (dfs/closure v visited)
    (unless (set-member? visited v)
      (set-add! visited v)
      (for ([son (in-set (hash-ref! g v (thunk (mutable-set))))])
        (dfs/closure son visited)
        (set-union! (hash-ref g v) (hash-ref! g son (thunk (mutable-set)))))))
  (let ([visited (mutable-set)])
    (for ([v (in-vertices g)])
      (dfs/closure v visited))))

; Deletes all vertices from mutable graph g.
(define (graph-clear! g)
  (hash-clear! g))

; Calls proc for each subclique of the graph g.
(define (enumerate-cliques g proc)

  (define (check candidates wrong)
    (for/or ([w (in-set wrong)])
      (for/and ([c (in-set candidates)])
        (connected? g w c))))

  (define (filter-unconnected set v)
    (list->mutable-set
     (for/list ([c (in-set set)]
                #:when (and (connected? g v c) (not (equal? c v))))
       c)))

  (define (bron-kerbosch cur-clique candidates wrong)
    (let loop ()
      (unless (or (set-empty? candidates) (check candidates wrong))
        (let* ([v (set-first candidates)]
               [new-candidates (filter-unconnected candidates v)]
               [new-wrong (filter-unconnected wrong v)])
          (set-add! cur-clique v)
          (cond
            [(and (set-empty? new-candidates) (set-empty? new-wrong))
             (proc cur-clique)]
            [else (bron-kerbosch cur-clique new-candidates new-wrong)])
          (set-remove! cur-clique v)
          (set-remove! candidates v)
          (set-add! wrong v)
          (loop)))))

  (bron-kerbosch (mutable-set) (list->mutable-set (vertices g)) (mutable-set)))
