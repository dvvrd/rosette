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
         (define (symbolize)
           (let ([constants (mutables:=symbolic!/memorize (mutations-of #'head))]
                 [head-constant (free-id-table-ref function-constants #'head)])
             (printf "Resulting constants: ~a\n" constants)
             (function-application->symbolic-constant head-constant
                                                      (list args ...)
                                                      (read-dependencies/current head-constant)
                                                      constants)))
         (cond
           [(already-speculated? #'head)
            (symbolize)]
           [(speculating-currently? #'head)
            (let ([head-constant (free-id-table-ref function-constants #'head)])
              (if (associated? #'head)
                  (let ([constants (mutables:=symbolic!/memorize (mutations-of #'head))])
                    (expression dependent-app (head-constant args ...) (read-dependencies/current head-constant) constants))
                  (with-call #'head (constant (gensym) (solvable-range (type-of head-constant))))))]
           [else
            (with-call #'head
              (let*-values ([(head-constant) (constant #'head type)]
                            [(_) (free-id-table-set! function-constants #'head head-constant)]
                            [(arg-constants) (box #f)]
                            [(state/before) (create-rollback-point)]
                            [(state/middle) (box #f)]
                            [(term-cache-snapshot) (box #f)]
                            [(impl) (λ ()
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
                                        (cons term state)))]
                            [(term state/after) (speculate/symbolized (impl))]
                            [(scoped-constants) (hash-values-diff constant? (term-cache) (unbox term-cache-snapshot))]) ; TODO: make it more effective with splicing-let
                (associate #'head state/after)
                (set-up-read-dependencies head-constant term (unbox arg-constants) scoped-constants state/after)
                (when (recursive? #'head)
                  (set!-values (term state/after) (speculate/symbolized (impl)))
                  (set! scoped-constants (hash-values-diff constant? (term-cache) (unbox term-cache-snapshot)))
                  (associate #'head state/after))
                (set-up-write-dependencies head-constant state/after)
                (eval/horn term
                           head-constant
                           (unbox arg-constants)
                           (set-subtract scoped-constants (list->set (unbox arg-constants)))
                           state/after)
                (symbolize)))])))]))

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
