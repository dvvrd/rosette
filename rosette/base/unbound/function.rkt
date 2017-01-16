#lang racket

(require
  syntax/id-table
  (for-syntax racket syntax/parse)
  (only-in "../core/bool.rkt" with-asserts)
  (only-in "../core/term.rkt"
           constant constant? expression type-of
           solvable-domain solvable-range
           term-cache clear-terms!)
  (only-in "contracts.rkt" λ/typed)
  (only-in "utils.rkt" hash-values-diff+filter gensym)
  (only-in "call-graph.rkt" with-call called? stack-size recursive? mutual-recursion-root?)
  "mutations.rkt" "dependencies.rkt" "encoding.rkt")

(provide (for-syntax make-solvable-function) instantiate-high-order-solvable-function)

(define-for-syntax (make-solvable-function head args type body)
  #`(with-handlers ([exn:fail?
                     (λ (err)
                       (λ (#,@args)
                         (raise err)))])
      (let* ([head-syntax #,head]
             [type #,type])
        (λ/typed (#,@args) type
          (cond
            [(already-speculated? head-syntax)
             (symbolize head-syntax (list #,@args))]
            [(speculating-currently? head-syntax)
             (symbolize-if-associated head-syntax (list #,@args))]
            [else
             (with-call head-syntax
               (let* ([head-constant (constant (syntax->datum head-syntax) type)]
                      [_ (free-id-table-set! applicable-constants head-syntax head-constant)]
                      [_ (associate-id head-syntax head-constant)]
                      [arg-constants (box #f)]
                      [state/before (state-before-call)]
                      [state/middle (box #f)]
                      [term-cache-snapshot (box #f)]
                      [impl (λ ()
                              (cond
                                [(unbox state/middle)
                                 (restore-symbolization (unbox state/middle))]
                                [else
                                 (mutables:=symbolic!/track head-constant state/before)
                                 (set-box! state/middle (create-rollback-point))])
                              #,@(for/list ([arg (syntax->list args)]
                                            [i (in-naturals)])
                                   #`(define #,arg (constant (gensym '#,arg) (i-th-member-of-domain #,i type))))
                              (set-box! arg-constants (list #,@args))
                              (unless (unbox term-cache-snapshot)
                                (set-box! term-cache-snapshot (hash-copy (term-cache))))
                              (let-values ([(term state) (speculate* (#,body))])
                                (cons term state)))])
                 (solvable-function->horn-clauses head-syntax head-constant
                                                  (list #,@args) arg-constants
                                                  impl term-cache-snapshot)))])))))

(define (i-th-member-of-domain i type)
  (let ([domain (solvable-domain type)])
    (if (< i (length domain))
        (list-ref domain i)
        (error 'define "Too many arguments!"))))

(define-syntax-rule (speculate/symbolized body)
  (let-values
      ([(pair assertions)
        (with-asserts
            (let-values ([(pair _) (speculate* body)])
              pair))])
    (values (car pair) (cdr pair) assertions)))

(define (already-speculated? id) (and (dict-has-key? applicable-constants id) (not (called? id))))
(define (speculating-currently? id) (called? id))
(define (call-tree-root?) (equal? (stack-size) 2))

(define high-order-functions (make-hash))

(define (instantiate-high-order-solvable-function id functions constructor)
  (if (empty? functions)
      (constructor)
      (hash-ref! high-order-functions (cons id functions) constructor)))

(define applicable-constants (make-free-id-table))
(define delimited-encodings (make-parameter (list)))
(define current-state-before-call (make-parameter #f))

(define (state-before-call)
  (when (call-tree-root?)
    (current-state-before-call (create-rollback-point)))
  (current-state-before-call))

(define (symbolize head args)
  (let* ([head-constant (free-id-table-ref applicable-constants head)]
         [read-dependencies (read-dependencies/current head-constant)]
         [constants (mutables:=symbolic!/memorize (write-dependencies-states head))])
    (function-application->symbolic-constant head-constant
                                             args
                                             read-dependencies
                                             constants)))

(define (symbolize-if-associated head args)
  (let ([head-constant (free-id-table-ref applicable-constants head)])
    (if (read-dependencies-ready? head)
        (let ([read-dependencies (read-dependencies/current head-constant)]
              [constants (mutables:=symbolic!/memorize (write-dependencies-states head-constant))])
          (expression dependent-app
                      (apply head-constant args)
                      read-dependencies
                      constants))
        (with-call head
          (apply (constant (gensym) (type-of head-constant)) args)))))

(define (solvable-function->horn-clauses head head-constant
                                         args arg-constants
                                         body term-cache-snapshot)
  (define-values (term state/after assertions) (speculate/symbolized (body)))
   ; TODO: make it more effective
  (define scoped-constants (hash-values-diff+filter constant? (term-cache) (unbox term-cache-snapshot)))
  (set-up-read-dependencies head-constant
                            term
                            (unbox arg-constants)
                            scoped-constants
                            state/after)
  (set-up-write-dependencies head-constant state/after)
  (define (delimited-encoding)
    (when (recursive? head)
      ; For recusive functions we need second execution, because we know set of write-dependencies only now.
      (set!-values (term state/after assertions) (speculate/symbolized (body)))
      (set! scoped-constants (hash-values-diff+filter constant? (term-cache) (unbox term-cache-snapshot)))
      (set-up-read-dependencies head-constant
                                term
                                (unbox arg-constants)
                                scoped-constants
                                state/after)
      (set-up-write-dependencies head-constant state/after))
    (eval/horn term
               head-constant
               (unbox arg-constants)
               assertions))
  (cond [(recursive? head)
         (delimited-encodings (cons delimited-encoding (delimited-encodings)))
         (cond
           [(mutual-recursion-root?)
            (eval-delimited-encodings head args)
            (symbolize head args)]
           [else
            (constant (gensym) (solvable-range (type-of head-constant)))])]
        [else
         (delimited-encoding)
         (symbolize head args)]))


(define (eval-delimited-encodings head args)
  (for ([enc (reverse (delimited-encodings))]) (enc))
  (delimited-encodings (list)))
