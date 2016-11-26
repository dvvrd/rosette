
#lang racket

(require racket/dict syntax/id-table syntax/id-set)

(provide with-call called? stack-size recursive? mutual-recursion-root?
         make-associations associate associated? reset-associations-cache fold/reachable)

;; ----------------- Call graph ----------------- ;;

; Maps id to its callees
(define call-graph (make-free-id-table))

(define (connect v1 v2)
  (free-id-set-add! (free-id-table-ref! call-graph v1 (mutable-free-id-set)) v2))

(define (in-callees v)
  (in-free-id-set (free-id-table-ref call-graph v (immutable-free-id-set))))

;; ----------------- Call stack ----------------- ;;

(define current-call-stack (make-parameter (list (box (cons #'%:main #f)))))

; Updates the internal information about the call graph evaluating the given bodies.
(define-syntax-rule (with-call id body body-rest ...)
  (let ([caller (car (unbox (car (current-call-stack))))])
    (detect-recursion id)
    (parameterize ([current-call-stack (cons (box (cons id #f))
                                             (current-call-stack))])
      (connect caller id)
      body
      body-rest ...)))

; Returns #f if 'id is not currently in a call stack or a tail of a call stack
; below the last call of 'id otherwise.
(define (called? id)
  (member id (current-call-stack)
          (λ (id frame)
            (free-identifier=? id
                               (car (unbox frame))))))

; Returns a number of entries in current call stack. Note that top-level context is also included
; into call-stack.
(define (stack-size)
  (length (current-call-stack)))

;; ----------------- Recursion detection ----------------- ;;

(define recursive-functions (mutable-free-id-set))

(define (detect-recursion id)
  (when (called? id)
    (let ([mutual-recursion-component
           (dropf-right (current-call-stack)
                        (λ (frame)
                          (not
                           (free-identifier=?
                            id
                            (car (unbox frame))))))])
      (for ([frame mutual-recursion-component])
        (free-id-set-add! recursive-functions (car (unbox frame)))
        (set-box! frame (cons (car (unbox frame)) #f)))
      (unless (empty? mutual-recursion-component)
        (set-box! (last mutual-recursion-component) (cons id #t))))))

; Returns #t if 'id is an identifier of (mutually) recursive function.
(define (recursive? id)
  (free-id-set-member? recursive-functions id))

; Returns #t if current callstack top is a (mutually) recusive function and
; if it was called by function that is not (mutually) recursive.
; In other words, returns #f if and only if current call stack top is call
; in the middle of mutually recursive sequence of calls.
(define (mutual-recursion-root?)
  (cdr (unbox (car (current-call-stack)))))

;; ----------------- Associations ----------------- ;;

(struct associations (storage cache))

; Associations is a data structure that allows associating some arbitrary
; data with call graph vertices (i.e. solvable functions). The associated
; information can be then folded with fold/reachable. The results of folding
; will be cached in associations structure itself.
; make-associations returns fresh associations cache.
(define (make-associations)
  (associations (make-free-id-table) (make-free-id-table)))

; Erases all data memorized for fold/reachable queries.
(define (reset-associations-cache associations)
  (dict-clear! (associations-cache associations)))

; Overwrites the current value associated with 'id.
(define (associate associations id info)
  (free-id-table-set! (associations-storage associations) id info))

; Returns true if associations cache contains some info associated with 'id.
(define (associated? associations id)
  (dict-has-key? (associations-storage associations) id))

; Same as foldl, but folds associations with all reachable from 'id vertices.
; The results of folding are cached, so next time when some caller of 'id
; will be folded, reacable from 'id vertices will not be traversed one more time.
; Instead, prevoious results will be reused. That means that the set of reachable
; from 'id vertices should not be changed between fold/reachable calls. If it does
; reset-associations-cache should be called.
(define (fold/reachable associations id proc)
  (let ([storage (associations-storage associations)]
        [cache (associations-cache associations)])
    (if (dict-has-key? cache id)
        (free-id-table-ref cache id)
        (let* ([current-assoc (free-id-table-ref! storage id (mutable-free-id-set))]
               [_ (free-id-table-set! cache id current-assoc)]
               [result (for/fold ([acc current-assoc])
                                 ([v (in-callees id)])
                         (proc acc (fold/reachable associations v proc)))])
          (free-id-table-set! cache id result)
          result))))
