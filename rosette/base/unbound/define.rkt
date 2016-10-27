#lang racket

(require
  syntax/id-table
  (for-syntax racket syntax/parse)
  (only-in "../core/term.rkt"
           constant constant? expression
           solvable-domain solvable-range term-cache type-of)
  (only-in "../form/define.rkt" define-symbolic)
  (only-in "utils.rkt" hash-values-diff+filter)
  (only-in "call-graph.rkt" with-call called? stack-size recursive?)
  "mutations.rkt" "dependencies.rkt" "encoding.rkt")

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
                     [_ (free-id-table-set! applicable-constants #'head head-constant)]
                     [_ (associate-id #'head head-constant)]
                     [arg-constants (box #f)]
                     [state/before (state-before-call)]
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

(define-syntax-rule (speculate/symbolized body)
  (let-values ([(pair _) (speculate* body)])
    (values (car pair) (cdr pair))))

(define (already-speculated? id) (and (dict-has-key? applicable-constants id) (not (called? id))))
(define (speculating-currently? id) (called? id))
(define (call-tree-root?) (equal? (stack-size) 2))

(define applicable-constants (make-free-id-table))
(define delimited-encodings (make-parameter (list)))
(define current-state-before-call (make-parameter #f))

(define (state-before-call)
  (when (call-tree-root?)
    (current-state-before-call (create-rollback-point)))
  (current-state-before-call))

(define (symbolize head args)
  (let ([constants (mutables:=symbolic!/memorize (write-dependencies-states head))]
        [head-constant (free-id-table-ref applicable-constants head)])
    (unless (null? constants)
      (printf "Resulting constants: ~a\n" constants))
    (function-application->symbolic-constant head-constant
                                             args
                                             (read-dependencies/current head-constant)
                                             constants)))

(define (symbolize-if-associated head args)
  (let ([head-constant (free-id-table-ref applicable-constants head)])
    (if (read-dependencies-ready? head)
        (let ([constants (mutables:=symbolic!/memorize (write-dependencies-states head-constant))])
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
  (define scoped-constants (hash-values-diff+filter constant? (term-cache) (unbox term-cache-snapshot)))
  (set-up-read-dependencies head-constant term (unbox arg-constants) scoped-constants state/after)
  (set-up-write-dependencies head-constant state/after)
  (define (delimited-encoding)
    (when (recursive? head)
      ; For recusive functions we need second execution, because we know set of write-dependencies only now.
      (set!-values (term state/after) (speculate/symbolized (body)))
      (set! scoped-constants (hash-values-diff+filter constant? (term-cache) (unbox term-cache-snapshot)))
      (set-up-write-dependencies head-constant state/after))
    (eval/horn term
               head-constant
               (unbox arg-constants)
               (set-subtract scoped-constants (list->set (unbox arg-constants)))))
  (cond [(recursive? head)
         (delimited-encodings (cons delimited-encoding (delimited-encodings)))
         (cond
           [(call-tree-root?)
            (for ([enc (reverse (delimited-encodings))]) (enc))
            (symbolize head args)]
           [else
            (constant (gensym) (solvable-range (type-of head-constant)))])]
        [else (delimited-encoding)]))
