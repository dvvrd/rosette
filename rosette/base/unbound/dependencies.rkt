#lang racket

; This module controls read and write dependencies of functions
; declared via define/unbound. Read dependencies are locations
; accessed with function or by one of its callees for reading.
; Write dependencies are locations updated by function body or
; by one of its callees.

(require
  (only-in "../core/term.rkt" constant? define-operator type-of type-applicable?)
  (only-in "mutations.rkt" state->mutations symbolization->actual-value)
  (only-in "utils.rkt" terms->constants))

(provide read-dependencies/original read-dependencies/current write-dependencies
         set-up-read-dependencies set-up-write-dependencies dependent-app)

(define read-dependencies-cache (make-hash))
(define write-dependencies-cache (make-hash))

(define (encapsulate-read-dependence const)
  (λ (original?)
    (if original? const (symbolization->actual-value const))))

(define (write-dependencies f)
  (hash-ref write-dependencies-cache f (list)))

(define (read-dependencies/original f)
  (map (λ (dep) (dep #t))
       (hash-ref read-dependencies-cache f)))

(define (read-dependencies/current f)
  (map (λ (dep) (dep #f))
       (hash-ref read-dependencies-cache f)))

(define (set-up-write-dependencies f mutations-state)
  (hash-set! write-dependencies-cache f (state->mutations mutations-state)))

(define (set-up-read-dependencies f body args scope mutations-state)
  (let* ([mutations (state->mutations mutations-state)]
         [constants (terms->constants (cons body mutations))]
         [global-constants (filter (λ (v) (global-var? v args scope))
                                   (set->list constants))])
    (hash-set! read-dependencies-cache f
               (map encapsulate-read-dependence global-constants))))

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

;(define (intermediate-var? v)
;  (regexp-match #rx"^ε[0-9]+$" (format "~a" v)))

(define (global-var? v [args #f] [scope #f])
  (and (constant? v)
       (not (or (argument? v args)
                ;(intermediate-var? v)
                (type-applicable? (type-of v))
                (local-var? v scope)))))
