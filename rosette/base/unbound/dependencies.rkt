#lang racket

; This module controls read and write dependencies of functions
; declared via define/unbound. Read dependencies are locations
; accessed with function or by one of its callees for reading.
; Write dependencies are locations updated by function body or
; by one of its callees.

(require
  (only-in "../core/term.rkt" constant? define-operator type-of type-applicable?)
  (only-in "mutations.rkt" state->mutations symbolization->actual-value state=? source-location=? symbolization-of-head)
  (only-in "utils.rkt" terms->constants)
  (only-in "call-graph.rkt" make-associations associate associated? reset-associations-cache fold/reachable))

(provide read-dependencies/original read-dependencies/current
         write-dependencies write-dependencies-states
         set-up-read-dependencies set-up-write-dependencies
         read-dependencies-ready? dependent-app associate-id)

(define constants-to-ids (make-hash))
(define read-dependencies-cache (make-associations))
(define write-dependencies-cache (make-associations))

(define (write-dependencies-states f)
  (fold/reachable write-dependencies-cache (constant->id f) write-dependencies-union))

(define (write-dependencies f)
  (state->mutations (write-dependencies-states f)))

(define (read-dependencies/original f)
  (map (curry symbolization-of-head f)
       (fold/reachable read-dependencies-cache (constant->id f) read-dependencies-union)))

(define (read-dependencies/current f)
  (map symbolization->actual-value (read-dependencies/original f)))

(define (associate-id id constant)
  (hash-set! constants-to-ids constant id))

(define (constant->id constant)
  (hash-ref constants-to-ids constant constant))

(define (set-up-write-dependencies f mutations-state)
  (reset-associations-cache write-dependencies-cache)
  (associate write-dependencies-cache (constant->id f) mutations-state))

(define (set-up-read-dependencies f body args scope mutations-state)
  (let* ([mutations (state->mutations mutations-state)]
         [constants (terms->constants (cons body mutations))]
         [global-constants (filter (λ (v) (global-var? v args scope))
                                   (set->list constants))])
    (associate read-dependencies-cache (constant->id f) global-constants)))

(define (read-dependencies-ready? f)
  (associated? read-dependencies-cache (constant->id f)))

; Wraps usual @app providing additional specification of state dependencies
; and mutations that @app results.
(define-operator dependent-app
  #:identifier 'dependent-app
  #:range (λ (expr read-dependencies write-dependencies) (type-of expr))
  #:unsafe (λ (expr read-dependencies write-dependencies) expr))

(define (argument? v [args #f])
  (and args (member v args)))

(define (local-var? v [scope #f])
  (and scope (set-member? scope v)))

(define (global-var? v [args #f] [scope #f])
  (and (constant? v)
       (not (or (argument? v args)
                (type-applicable? (type-of v))
                (local-var? v scope)))))

(define (read-dependencies-union d1 d2)
  (remove-duplicates (append d1 d2)
                     (λ (s1 s2) (or (equal? s1 s2)
                                    (source-location=? s1 s2)))))

(define (write-dependencies-union d1 d2)
  (remove-duplicates (append d1 d2) state=?))
