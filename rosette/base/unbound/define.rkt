#lang racket

(require
  syntax/id-table
  (for-syntax racket syntax/parse)
  (only-in "../core/term.rkt"
           constant constant? expression
           solvable-domain solvable-range term-cache type-of)
  (only-in "../form/define.rkt" define-symbolic)
  "call-graph.rkt" "mutations.rkt" "dependencies.rkt" "encoding.rkt")

(provide define/unbound rules->assertions)

(define-syntax (define/unbound stx)
  (syntax-parse stx
    [(_ (head args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (define (head args ...)
         (cond
           [(already-speculated? #'head)
            (symbolize #'head (list args ...))]
           [(speculating-currently? #'head)
            (symbolize-if-associated #'head (list args ...))]
           [else
            (with-call #'head
              (let* ([head-constant (constant #'head type)]
                     [_ (free-id-table-set! function-constants #'head head-constant)]
                     [arg-constants (box #f)]
                     [state/before (create-rollback-point)]
                     [state/middle (box #f)]
                     [term-cache-snapshot (box #f)]
                     [impl (λ ()
                               (cond
                                 [(unbox state/middle)
                                  (restore-symbolization (unbox state/middle))]
                                 [else
                                  (mutables:=symbolic!/track state/before)
                                  (set-box! state/middle (create-rollback-point))])
                               #,@(for/list ([arg (syntax->list #'(args ...))]
                                             [i (in-naturals)])
                                    #`(define-symbolic #,arg (i-th-member-of-domain #,i type)))
                               (set-box! arg-constants (list args ...))
                               (set-box! term-cache-snapshot (hash-copy (term-cache)))
                               (let-values ([(term state) (speculate* ((thunk body body-rest ...)))])
                                 (cons term state)))])
                (solvable-function->horn-clauses #'head head-constant
                                                 (list args ...) arg-constants
                                                 impl term-cache-snapshot)))])))]))

(define (i-th-member-of-domain i type)
  (let ([domain (solvable-domain type)])
    (if (< i (length domain))
        (list-ref domain i)
        (error 'define "Too many arguments!"))))


(define (hash-values-diff filter-func hash1 hash2)
  (let ([values1 (apply set (filter filter-func (hash-values hash1)))]
        [values2 (apply set (filter filter-func (hash-values hash2)))])
    (set-subtract values1 values2)))

(define-syntax-rule (speculate/symbolized body)
  (let-values ([(pair _) (speculate* body)])
    (values (car pair) (cdr pair))))

(define (already-speculated? id) (and (associated? id) (not (called? id))))
(define (speculating-currently? id) (called? id))

(define (mutations-of id)
  (fold/reachable id state-union))

(define function-constants (make-free-id-table))

(define (symbolize head args)
  (let ([constants (mutables:=symbolic!/memorize (mutations-of head))]
        [head-constant (free-id-table-ref function-constants head)])
    (unless (null? constants)
      (printf "Resulting constants: ~a\n" constants))
    (function-application->symbolic-constant head-constant
                                             args
                                             (read-dependencies/current head-constant)
                                             constants)))

(define (symbolize-if-associated head args)
  (let ([head-constant (free-id-table-ref function-constants head)])
    (if (associated? head)
        (let ([constants (mutables:=symbolic!/memorize (mutations-of head))])
          (expression dependent-app
                      (apply head-constant args)
                      (read-dependencies/current head-constant)
                      constants))
        (with-call head
          (constant (gensym) (solvable-range (type-of head-constant)))))))

(define (solvable-function->horn-clauses head head-constant
                                         args arg-constants
                                         body term-cache-snapshot)
  (define-values (term state/after) (speculate/symbolized (body)))
   ; TODO: make it more effective
  (define scoped-constants (hash-values-diff constant? (term-cache) (unbox term-cache-snapshot)))
  (associate head state/after)
  (set-up-read-dependencies head-constant term (unbox arg-constants) scoped-constants state/after)
  (define (delimited-encoding)
    (when (recursive? head)
      ; For recusive functions we need second execution, because we know set of write-dependencies only now.
      (set!-values (term state/after) (speculate/symbolized (body)))
      (set! scoped-constants (hash-values-diff constant? (term-cache) (unbox term-cache-snapshot)))
      (associate head state/after))
    (set-up-write-dependencies head-constant state/after)
    (eval/horn term
               head-constant
               (unbox arg-constants)
               (set-subtract scoped-constants (list->set (unbox arg-constants)))
               state/after)
    (symbolize head args))
  (delimited-encoding))
