#lang racket

(require
  (for-syntax racket racket/syntax syntax/parse
              (only-in "rules.rkt" dbg current-head))
  (only-in "../core/term.rkt" type-of constant expression)
  (only-in "../core/bool.rkt" @!)
  (only-in "../form/control.rkt" branch-and-merge)
  "rules.rkt")

; !!!!!!! TODO: Delete it when types implemented !!!!!!!
(require (only-in "../core/function.rkt" ~>)         ;!!
         (only-in "../core/real.rkt" @integer? @real?))     ;!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


(provide @define rules->assertions)

; Overrides racket define. If procedure or lambda-expression
; is defined traverses its body and creates a set of Horn clauses describing it.
; Horn clauses converted later into horn-solver smt2 input format.
(define-syntax (@define stx)
  (syntax-parse stx
    [(_ (head args ...) body body-rest ...)
     (parameterize ([current-intermediate-vars-count (current-intermediate-vars-count)])
       (quasisyntax/loc stx
         (define (head args ...)
           (parameterize* ([current-head #'head]
                           [current-args
                            (map (λ (arg)
                                   (argument->symbolic-constant arg
                                                                (type-of 0))) ; TODO: TYPES!
                                 (syntax->list #'(args ...)))]
                           [current-free-vars (append (current-args) (current-free-vars))]
                           [current-premises (current-premises)])
             ((delay-expansion
               head body body-rest ...))
             (apply expression
                    `(, @app
                      , (recursive-function->symbolic-constant #'head (~> @integer? @integer?)) ; TODO: TYPES!
                      ,@(list args ...)))))))]
    [(_ id expr)
     (syntax/loc stx
       (define id expr))]))

; #%module-begin uses partial expansion of forms to detect definitions before expanding expressions.
; This is a second step of define expansion: expansion of procedure body.
(define-syntax (delay-expansion stx)
  (syntax-case stx ()
    [(_delay-expansion head more ...)
     (parameterize ([current-head #'head])
       (let* ([fully-expanded (local-expand #'(lambda () more ...) 'module (list))]
              [result (expression->horn-clauses #t fully-expanded)])
         (dbg "PRE-EXPANDED: ~a" result)
         result))]))

(define-for-syntax (list->horn-clauses tail-position? exprs)
  (let* ([exprs (syntax->list exprs)]
         [len (length exprs)])
    (for/list ([expr exprs] [i (in-naturals 1)])
      (expression->horn-clauses (and (= i len) tail-position?) expr))))

(define-for-syntax (if->horn-clauses tail-position? test-expr then-expr else-expr)
  (dbg "if ~a {~a} ~a ~a" tail-position? test-expr then-expr else-expr)
  (let* ([test-expr (expression->horn-clauses #f test-expr)]
         [then-expr (expression->horn-clauses tail-position? then-expr)]
         [else-expr (expression->horn-clauses tail-position? else-expr)])
    #`(branch-and-merge #,test-expr
                        (thunk
                         (parameterize ([current-premises (cons #,test-expr (current-premises))]
                                        [current-free-vars (current-free-vars)])
                           #,then-expr))
                        (thunk
                         (parameterize ([current-premises (cons (@! #,test-expr) (current-premises))]
                                        [current-free-vars (current-free-vars)])
                           #,else-expr)))))

(define-for-syntax (application->horn-clauses tail-position? proc-expr args)  
  (cond
    [(and (identifier? proc-expr) (free-identifier=? proc-expr #'branch-and-merge))
     (with-syntax ([(test-expr then-expr else-expr) args])
       (if->horn-clauses tail-position? #'test-expr (unthunk #'then-expr) (unthunk #'else-expr)))]
    [else
     ; TODO: understand when app returns void
     (let* ([args (list->horn-clauses #f args)]
            [result
             (if (and (identifier? proc-expr)
                      (free-transformer-identifier=? proc-expr (current-head)))
                 (let ([id (fresh-intermediate-var 'ε)])
                   #`(let ([#,id (constant '#,id (type-of 0))])
                       (current-free-vars (cons #,id (current-free-vars)))
                       (current-premises
                        (cons
                         (apply expression
                                (flatten
                                 (list @app
                                       (recursive-function->symbolic-relation (current-head) (~> @integer? @integer?)) ; TODO: TYPES!
                                       #,@args #,id)))
                         (current-premises)))
                       #,id))
                 #`(#%plain-app #,proc-expr #,@args))])
       (if tail-position?
           #`(let ([val #,result])
               (add-rule val)
               val)
           result))]))

(define-for-syntax (expression->horn-clauses tail-position? stx)
  (dbg "-------------------------------------------")
  (dbg "expression->horn-clauses ~a ~a" tail-position? stx)
  (syntax-parse
      (syntax-disarm stx orig-insp)
    #:literal-sets (kernel-literals)
    [var:id
     (dbg "id ~a" stx)
     #`(if (argument? #'#,stx)
           (argument->symbolic-constant #'#,stx (type-of 0))
           #,stx)]
    [(set! var expr) (dbg "set!!") '()]
    [(define-values (var) expr) (dbg "define-values!!") '()]
    [(define-values (var ...) expr) (dbg "define-values...!!") '()]
    [(let-values (initializers ...) body ...)
     (dbg "let-values [~a] ~a" #'(initializers ...) #'(body ...))
     (let ([initializers
            (map (λ (initializer)
                   (with-syntax ([((var ...) expr) initializer])
                     #`((var ...) #,(expression->horn-clauses #f #'expr))))
                 (syntax->list #'(initializers ...)))]
           [body (list->horn-clauses tail-position? #'(body ...))])
       #`(let-values (#,@initializers) #,@body))]
    [(letrec-values ([(var ...) expr] ...) body ...)
     (dbg "letrec-values [~a ~a] ~a" #'(var ... ...) #'(expr ...) #'(body ...))
     (list->horn-clauses #f #'(expr ...))
     (list->horn-clauses tail-position? #'(body ...))]
    [(letrec-syntaxes+values stx-decls ([(var ...) expr] ...) body ...)
     (dbg "letrec-syntaxes+values ~a [~a ~a] ~a" #'stx-decls #'(var ... ...) #'(expr ...) #'(body ...))
     (list->horn-clauses #f #'(expr ...))
     (list->horn-clauses tail-position? #'(body ...))]
    [(#%expression expr)
     (dbg "#%expression ~a" #'expr)
     (expression->horn-clauses tail-position? #'expr)]
    [(#%plain-lambda formals . rest)
     (dbg "#%plain-lambda ~a ~a" #'formals #'rest)
     #`(#%plain-lambda formals #,@(list->horn-clauses tail-position? #'rest))]
    [(#%plain-app proc-expr arg ...)
     (dbg "#%plain-app ~a ~a" #'proc-expr #'(arg ...))
     (application->horn-clauses tail-position? #'proc-expr #'(arg ...))]
    [(case-lambda . rest) (dbg "case-lambda!!") '()]
    [(if test-expr then-expr else-expr)
     (if->horn-clauses tail-position? tail-position? #'test-expr #'then-expr #'else-expr)]
    [(begin . rest) (dbg "begin!!") '()]
    [(begin0 . rest) (dbg "begin0!!") '()]
    [(with-continuation-mark . rest) (dbg "with-continuation-mark!!") '()]
    [(#%plain-module-begin . rest) (dbg "#%plain-module-begin!!") '()]
    [_
     (if tail-position? (dbg "return reached: ~a" stx) (dbg "just other term: ~a" stx))
     (if tail-position?
         #`(begin
             (add-rule #,stx)
             #,stx)
         stx)]))

(define-for-syntax orig-insp (variable-reference->module-declaration-inspector (#%variable-reference)))

(define-for-syntax (unthunk stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
    [(#%plain-lambda formals body body-rest ...) (syntax/loc stx body)]
    [_ stx]))


(define-for-syntax current-intermediate-vars-count (make-parameter 0))

(define-for-syntax (fresh-intermediate-var prefix)
  (begin0
    (format-id #f "~a~a" prefix (current-intermediate-vars-count))
    (current-intermediate-vars-count
     (add1 (current-intermediate-vars-count)))))
