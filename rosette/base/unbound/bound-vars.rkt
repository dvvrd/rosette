#lang racket

; When the set of Horn clauses is bult there is still one optimization
; that should be performed before encoding them into datalog format.
; Horn clauses obtained by transformations.rkt do NOT share bound vars.
; That will cause lots of unneeded (declare-var) statements in datalog
; encoding. To fix that we should make Horn clauses sharing bound vars.
; This module manages such shared bound vars (with considering their
; types), and replaces all occurences of bound vars into Horn clauses
; with shared ones.

(require
  racket/syntax
  (only-in "../core/term.rkt" constant constant? expression type-of)
  (only-in "relation.rkt" relation?)
  (only-in "utils.rkt" term->constants))

(provide bound-var? share-vars)

(define bound-var-suffix "$<bound-var>")
(define common-bound-vars (make-parameter (make-hash)))
(define common-bound-vars-count (make-parameter 0))

(define (bound-var? v)
  (and (constant? v) (string-suffix? (~a v) bound-var-suffix)))

(define (share-vars terms)
  (map share-vars/one terms))

(define (share-vars/one term)
  (substitute/constants
   (common-vars-substitution (term->constants term))
   term))

(define (substitute/constants subst t)
  (match t
    [(expression op args ...)
     (let ([args (map (curry substitute/constants subst) args)])
       (apply expression `(,op ,@args)))]
    [(? relation?) t]
    [(constant _ _) (hash-ref subst t t)]
    [_ t]))

(define (fill-in-insufficient common-vars n type)
  (cond [(<= n (length common-vars)) common-vars]
        [else
         (let* ([insifficient-count (- n (length common-vars))]
                [insufficient-vars (map (λ (i) (fresh-bound-var (+ (common-bound-vars-count) i) type))
                                        (range insifficient-count))])
           (append common-vars insufficient-vars))]))

(define (common-vars-substitution bound-vars)
  (let* ([grouped-vars (group-by type-of (set->list bound-vars))]
         [grouped-vars (for/hash ([group grouped-vars]
                                  #:when (not (empty? group)))
                         (values (type-of (car group)) group))])
    ; First filling in insufficient bound vars...
    (for ([type (in-hash-keys grouped-vars)])
      (let* ([existing (hash-ref (common-bound-vars) type (list))]
             [filled (fill-in-insufficient existing
                                           (length (hash-ref grouped-vars type))
                                           type)]
             [added-count (- (length filled) (length existing))])
        (unless (<= added-count 0)
          (hash-set! (common-bound-vars) type filled)
          (common-bound-vars-count (+ (common-bound-vars-count) added-count)))))
    ; ... and then producing substitution map
    (make-hash
     (apply append
            (for/list ([type (in-hash-keys grouped-vars)])
              (for/list ([v1 (hash-ref grouped-vars type)]
                         [v2 (hash-ref (common-bound-vars) type)])
                (cons v1 v2)))))))

(define (fresh-bound-var id type)
  (constant (format-id #f "x!~a~a" (number->string (add1 id)) bound-var-suffix) type))
