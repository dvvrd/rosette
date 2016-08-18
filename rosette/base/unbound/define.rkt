#lang racket

(require
  (for-syntax racket syntax/parse)
  "../base.rkt"
  "../form/control.rkt"
 )

(provide @define)

(begin-for-syntax
  (define-syntax-rule (dbg ft args ...)
    (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port))))
  

(define-for-syntax (lambda-body stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
    [(#%plain-lambda formals body body-rest ...) (syntax/loc stx body)]
    [_ stx]))

(define-for-syntax current-head (make-parameter #f))
(define-for-syntax current-premises (make-parameter (list)))

(define-for-syntax rules (make-hash))
(define-for-syntax (add-rule conclusion)
  (let* ([id (current-head)]
         [existing-clauses (hash-ref! rules id (list))]
         [current-clause #`(=> (and #,@(current-premises)) #,conclusion)])
    (dbg "adding rule ~a" current-clause)
    (hash-set! rules id (cons current-clause existing-clauses))))

(define-syntax (@define stx)
  (syntax-parse stx
    [(_ (head args ...) body body-rest ...)
     (parameterize ([current-head #'head])
       (let* ([ctx (syntax-local-make-definition-context)]
              [_ (syntax-local-bind-syntaxes (list #'head) #f ctx)]
              [resulting-define (syntax/loc stx (define (head args ...) body body-rest ...))]
              [fully-expanded (local-expand resulting-define 'top-level (list) ctx)])
         (dbg "(syntax-local-context): ~a" (syntax-local-context))
         (dbg "(resulting-define): ~a" resulting-define)
         (dbg "fully expanded: ~a" fully-expanded)
         (syntax-case fully-expanded ()
           [(define-values (ids ...) expr) (expression->horn-clauses #t (syntax/loc stx expr))])
         resulting-define))]
    [(_ id expr) #'(define id expr)]))

(define-for-syntax (list->horn-clauses tail-position? exprs)
  (let* ([exprs (syntax->list exprs)]
         [len (length exprs)])
    (for ([expr exprs] [i (in-naturals 1)]) (expression->horn-clauses (and (= i len) tail-position?) expr))))

(define-for-syntax (if->horn-clauses tail-position? test-expr then-expr else-expr)
  (dbg "if ~a {~a} ~a ~a" tail-position? test-expr then-expr else-expr)
  (expression->horn-clauses #f test-expr)
  (parameterize ([current-premises (cons test-expr (current-premises))])
    (expression->horn-clauses tail-position? then-expr))
  (parameterize ([current-premises (cons #`(not #,test-expr) (current-premises))])
    (expression->horn-clauses tail-position? else-expr)))

(define-for-syntax (application->horn-clauses tail-position? proc-expr args)  
  (cond
    [(and (identifier? #'id) (free-identifier=? proc-expr #'branch-and-merge))
       (with-syntax ([(test-expr then-expr else-expr) args])
         (if->horn-clauses tail-position? #'test-expr (lambda-body #'then-expr) (lambda-body #'else-expr)))]
    [else
       ; TODO: understand when app returns void
       (if tail-position?
           (add-rule #`(proc-expr #,@(syntax->list args)))
           (void))
       (list->horn-clauses #f args)]))

(define-for-syntax (expression->horn-clauses tail-position? stx)
  (let ([mock void]) ; TODO: DELETE IT!
  (dbg "-------------------------------------------")
  (dbg "expression->horn-clauses ~a ~a" tail-position? stx)
  (syntax-parse
    stx
    #:literal-sets (kernel-literals)
    [var:id (dbg "id") mock]
    [(set! var expr) (dbg "set!!") mock]
    [(define-values (var) expr) (dbg "define-values!!") mock]
    [(define-values (var ...) expr) (dbg "define-values...!!") mock]
    [(let-values ([(var ...) expr] ...) body ...)
       (dbg "let-values [~a ~a] ~a" #'(var ... ...) #'(expr ...) #'(body ...))
       (let ([premises #'((equal? (var ...) expr) ...)])
         (list->horn-clauses #f #'(expr ...))
         (parameterize ([current-premises (cons premises (current-premises))])
           (list->horn-clauses tail-position? #'(body ...))))
       mock]
    [(letrec-values ([(var ...) expr] ...) body ...)
       (dbg "letrec-values [~a ~a] ~a" #'(var ... ...) #'(expr ...) #'(body ...))
       (list->horn-clauses #f #'(expr ...))
       (list->horn-clauses tail-position? #'(body ...))
       mock]
    [(letrec-syntaxes+values stx-decls ([(var ...) expr] ...) body ...)
       (dbg "letrec-syntaxes+values ~a [~a ~a] ~a" #'stx-decls #'(var ... ...) #'(expr ...) #'(body ...))
       (list->horn-clauses #f #'(expr ...))
       (list->horn-clauses tail-position? #'(body ...))
       mock]
    [(#%expression expr)
       (dbg "#%expression ~a" #'expr)
       (expression->horn-clauses tail-position? #'expr)
       mock]
    [(#%plain-lambda formals . rest)
       (dbg "#%plain-lambda ~a ~a" #'formals #'rest)
       (list->horn-clauses tail-position? #'rest)
       mock]
    [(#%plain-app proc-expr arg ...)
       (dbg "#%plain-app ~a ~a" #'proc-expr #'(arg ...))
       (application->horn-clauses tail-position? #'proc-expr #'(arg ...))
       mock]
    [(case-lambda . rest) (dbg "case-lambda!!") mock]
    [(if test-expr then-expr else-expr)
       (if->horn-clauses tail-position? tail-position? #'test-expr #'then-expr #'else-expr)
       mock]
    [(begin . rest) (dbg "begin!!") mock]
    [(begin0 . rest) (dbg "begin0!!") mock]
    [(with-continuation-mark . rest) (dbg "with-continuation-mark!!") mock]
    [(#%plain-module-begin . rest) (dbg "#%plain-module-begin!!") mock]
    [_
       (if tail-position? (dbg "return reached: ~a" stx) (dbg "just other term: ~a" stx))
       (dbg "assertions: ~a" (current-premises))
       (if tail-position? (add-rule stx) (void))
       mock])))
