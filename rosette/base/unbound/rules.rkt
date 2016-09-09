#lang racket

(require
  racket/syntax syntax/id-table
  (only-in "../core/term.rkt"
           constant expression define-operator type-of
           @app solvable-domain solvable-range)
  (only-in "../core/bool.rkt" @boolean? @forall @=> @&&))


(provide dbg (struct-out horn-clause) rules->assertions @app add-rule
         recursive-function->symbolic-constant recursive?
         argument->symbolic-constant argument?
         recursive-function->symbolic-relation symbolic-constant->symbolic-relation
         current-head current-args current-free-vars current-premises)

(define-syntax-rule (dbg ft args ...)
  (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port)))
;  void)


(struct horn-clause (id free-vars premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port
              "~a(~a): [=> ~a ~a]\n"
              (syntax->datum (horn-clause-id self))
              (string-join
               (map
                (curry format "~a")
                (horn-clause-free-vars self))
               ", ")
              (horn-clause-premises self)
              (horn-clause-conclusion self)))])

(define rules (make-hash))

(define recursive-functions-cache (make-free-id-table))
(define relations-cache (make-free-id-table))
(define arguments-cache (make-free-id-table))

(define (recursive? f)
  (if (identifier? f)
      (or (member f (free-id-table-keys recursive-functions-cache) free-identifier=?)
          (member f (free-id-table-keys relations-cache) free-identifier=?))
      (member f (free-id-table-values recursive-functions-cache))))

(define (recursive-function->symbolic-constant f type)
  (free-id-table-ref! recursive-functions-cache f
                      ; Failure-expression should be wrapped into thunk, but it does not work for now
                      ; (http://bugs.racket-lang.org/query/?cmd=view&pr=15346).
                      ; TODO: wrap into thunk when fixed
                      ;             (thunk
                      (constant (syntax->datum f) type)))

(define (recursive-function->symbolic-relation f func-type)
  (free-id-table-ref! relations-cache f
                      ; Failure-expression should be wrapped into thunk, but it does not work for now
                      ; (http://bugs.racket-lang.org/query/?cmd=view&pr=15346).
                      ; TODO: wrap into thunk when fixed
                      ;             (thunk
                      (constant (syntax->datum (format-id #f "~a°" f)) (function-type->relational-type func-type))))

(define (symbolic-constant->symbolic-relation f)
  (let ([id (free-id-table-key recursive-functions-cache f)])
    (unless id
      (error 'rules "Symbolic constant for ~a not registered" f))
    (recursive-function->symbolic-relation id (type-of f))))

(define (argument? id)
  (member id (free-id-table-keys arguments-cache) free-identifier=?))

(define (argument->symbolic-constant arg type)
  (free-id-table-ref! arguments-cache arg
                      ; Failure-expression should be wrapped into thunk, but it does not work for now
                      ; (http://bugs.racket-lang.org/query/?cmd=view&pr=15346).
                      ; TODO: wrap into thunk when fixed
                      ;             (thunk
                      (constant (syntax->datum arg) type)))

; !!!!!!! TODO: Delete it when types implemented !!!!!!!
(require (only-in "../core/function.rkt" ~>)         ;!!
         (only-in "../core/real.rkt" @integer?))     ;!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

; Takes conclusion and constructs horn-clause using current traversal context.
(define (add-rule value)
  (let* ([id (current-head)]
         [existing-clauses (hash-ref! rules id (list))]
         [conclusion (apply expression
                            `(, @app
                              , (recursive-function->symbolic-relation (current-head) (~> @integer? @integer?)) ; TODO: TYPES!
                              ,@(current-args)
                              ,value))]
         [resulting-clause (horn-clause (current-head) (current-free-vars) (current-premises) conclusion)])
    (unless (member resulting-clause existing-clauses)
      (dbg "ADDING RULE ~a" resulting-clause)
      (hash-set! rules id (cons resulting-clause existing-clauses)))))

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-free-vars (make-parameter (list)))
(define current-premises (make-parameter (list)))

(define (rule->implication clause)
  (expression @=>
              (apply expression
                     `(, @&&
                       ,@(flatten (horn-clause-premises clause))))
              (horn-clause-conclusion clause)))

(define-syntax rules->assertions
  (syntax-rules ()
    [(_ rules)
     (map (λ (rule)
            (if (horn-clause? rule)
                (expression @forall (horn-clause-free-vars rule) (rule->implication rule))
                rule))
          rules)]
    [(_) (rules->assertions (flatten (hash-values rules)))]))

; Transforms (@integer? ~> @integer?) to (@integer? ~> @integer? ~> @boolean?)
(define (function-type->relational-type t)
  (apply ~> `(,@(solvable-domain t) ,(solvable-range t) , @boolean?)))

(define (free-id-table-key table value)
  (for/or ([(key val) (in-free-id-table table)])
    (and (equal? val value) key)))
