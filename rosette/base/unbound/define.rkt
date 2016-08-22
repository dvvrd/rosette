#lang racket

(require
  (for-syntax racket syntax/parse "rules.rkt")
  "../base.rkt"
  "../form/control.rkt"
  "rules.rkt"
 )

(provide @define rules->assertions)

; Takes conclusion and constructs horn-clause using current traversal context.
(define-for-syntax (make-rule conclusion) (horn-clause (current-head) (current-premises) conclusion))

(define-for-syntax current-head (make-parameter #f))
(define-for-syntax current-premises (make-parameter (list)))

; Overrides racket define. If procedure or lambda-expression
; is defined traverses its body and creates a set of Horn clauses describing it.
; Horn clauses converted later into horn-solver smt2 input format.
(define-syntax (@define stx)
  (syntax-parse stx
    [(_ (head args ...) body body-rest ...)
     (parameterize ([current-head #'head])
       (let* ([ctx (syntax-local-make-definition-context)]
              [_ (syntax-local-bind-syntaxes (list #'head) #f ctx)]
              [resulting-define (syntax/loc stx (define (head args ...) body body-rest ...))]
              [fully-expanded (local-expand resulting-define 'top-level (list) ctx)]
              [resulting-clauses (with-syntax ([(define-values (ids ...) expr) fully-expanded])
                                   (expression->horn-clauses #t (syntax/loc stx expr)))])
         (dbg "(resulting rules): ~a" resulting-clauses)
         (quasisyntax/loc stx
           (begin
             #,@(map (λ (clause) #`(add-rule #,(thunk (horn-clause-id clause)) #,(thunk (horn-clause-premises clause)) #,(thunk (horn-clause-conclusion clause)))) resulting-clauses)
             #,resulting-define))))]
    [(_ id expr) #'(define id expr)]))

(define-for-syntax (list->horn-clauses tail-position? exprs)
  (let* ([exprs (syntax->list exprs)]
         [len (length exprs)]
         [non-flat-result (for/list ([expr exprs] [i (in-naturals 1)]) (expression->horn-clauses (and (= i len) tail-position?) expr))])
    (flatten non-flat-result)))

(define-for-syntax (if->horn-clauses tail-position? test-expr then-expr else-expr)
  (dbg "if ~a {~a} ~a ~a" tail-position? test-expr then-expr else-expr)
  (expression->horn-clauses #f test-expr)
  (let ([then-clauses
         (parameterize ([current-premises (cons test-expr (current-premises))])
           (expression->horn-clauses tail-position? then-expr))]
        [else-clauses
         (parameterize ([current-premises (cons #`(not #,test-expr) (current-premises))])
           (expression->horn-clauses tail-position? else-expr))])
    (append then-clauses else-clauses)))

(define-for-syntax (application->horn-clauses tail-position? proc-expr args)  
  (cond
    [(and (identifier? #'id) (free-identifier=? proc-expr #'branch-and-merge))
       (with-syntax ([(test-expr then-expr else-expr) args])
         (if->horn-clauses tail-position? #'test-expr (unthunk #'then-expr) (unthunk #'else-expr)))]
    [else
       ; TODO: understand when app returns void
       (begin0
         ; TODO: change order, this is just for debug
         (if tail-position?
           (list (make-rule #`(proc-expr #,@(syntax->list args))))
           '())
         (list->horn-clauses #f args))]))

(define-for-syntax (expression->horn-clauses tail-position? stx)
  (dbg "-------------------------------------------")
  (dbg "expression->horn-clauses ~a ~a" tail-position? stx)
  (syntax-parse
    (syntax-disarm stx orig-insp)
    #:literal-sets (kernel-literals)
    [var:id (dbg "id") '()]
    [(set! var expr) (dbg "set!!") '()]
    [(define-values (var) expr) (dbg "define-values!!") '()]
    [(define-values (var ...) expr) (dbg "define-values...!!") '()]
    [(let-values ([(var ...) expr] ...) body ...)
       (dbg "let-values [~a ~a] ~a" #'(var ... ...) #'(expr ...) #'(body ...))
       (let ([premises #'((equal? (var ...) expr) ...)])
         (list->horn-clauses #f #'(expr ...))
         (parameterize ([current-premises (cons premises (current-premises))])
           (list->horn-clauses tail-position? #'(body ...))))]
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
       (list->horn-clauses tail-position? #'rest)]
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
       (dbg "assertions: ~a" (current-premises))
       (if tail-position? (list (make-rule stx)) '())]))

(define-for-syntax orig-insp (variable-reference->module-declaration-inspector (#%variable-reference)))

(define-for-syntax (unthunk stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
    [(#%plain-lambda formals body body-rest ...) (syntax/loc stx body)]
    [_ stx]))
