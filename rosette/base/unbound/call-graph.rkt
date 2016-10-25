#lang racket

(require racket/dict syntax/id-table syntax/id-set)

(provide with-call called? associate associated? fold/reachable recursive?)

;; ----------------- Call graph ----------------- ;;

; Maps id to its callees
(define call-graph (make-free-id-table))

(define (connect v1 v2)
  (free-id-set-add! (free-id-table-ref! call-graph v1 (mutable-free-id-set)) v2))

(define (in-callees v)
  (in-free-id-set (free-id-table-ref call-graph v (immutable-free-id-set))))

;; ----------------- Call stack ----------------- ;;

(define current-call-stack (make-parameter (list #'main)))

; Updates the internal information about the call graph evaluating the given bodies.
(define-syntax-rule (with-call id body body-rest ...)
  (let ([caller (last (current-call-stack))])
    (detect-recursion id)
    (parameterize ([current-call-stack (cons id (current-call-stack))])
      (connect caller id)
      body
      body-rest ...)))

; Returns #f if 'id is not currently in a call stack or a tail of a call stack
; below the last call of 'id otherwise.
(define (called? id)
  (member id (current-call-stack) free-identifier=?))

;; ----------------- Recursion detection ----------------- ;;

(define recursive-functions (mutable-free-id-set))

(define (detect-recursion id)
  (when (called? id)
    (for ([call (current-call-stack)])
      (free-id-set-add! recursive-functions call))))

; Returns #t if 'id is an identifier of (mutually) recursive function.
(define (recursive? id)
  (free-id-set-member? recursive-functions id))

;; ----------------- Associations ----------------- ;;

(define associations (make-free-id-table))
(define associations* (make-free-id-table))

; Overwrites the current value associated with 'id.
(define (associate id info)
  (free-id-table-set! associations id info))

; Returns true if associations cache contains some info associated with 'id.
(define (associated? id)
  (dict-has-key? associations id))

; Same as foldl, but folds associations with all reachable from 'id vertices.
; The results of floding are cached, so next time when some caller of 'id
; will be folded, reacable from 'id vertices will not be traversed one more time.
; Instead, prevoious results will be reused. That means that the set of reachable
; from 'id vertices should not be changed between fold/reachable calls, but this
; will never happen with CALL graph (because during the evaluation of body of 'id
; we already visited every reachable vertice).
(define (fold/reachable id proc)
  (if (dict-has-key? associations* id)
      (free-id-table-ref associations* id)
      (let ([current-assoc (free-id-table-ref! associations id (mutable-free-id-set))])
        (free-id-table-set! associations* id current-assoc)
        (for/fold ([acc current-assoc])
                  ([v (in-callees id)])
          (proc acc (fold/reachable v proc))))))
