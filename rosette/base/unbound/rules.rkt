#lang racket

(require
  racket/syntax
  (only-in "../core/term.rkt"
           define-operator constant constant? expression
           type-of @app solvable-domain solvable-range type-applicable?)
  (only-in "../core/polymorphic.rkt"
           ite ite* guarded)
  (only-in "../core/bool.rkt" @boolean? @! @=> @&&)
  (only-in "../core/function.rkt" ~>)
  (only-in "mutations.rkt" state->mutations symbolization->actual-value))

(provide dbg @app (struct-out horn-clause)
         register-solvable-function
         rules->assertions term->rules (rename-out [eval-body/horn eval/horn])
         function-application->symbolic-constant
         bound-var? relation?)

(define-syntax-rule (dbg ft args ...)
  (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port)))
;  void)

(define (for**/list lists proc)
  (define (for**/list-rec lists proc current result)
    (let ([h (car lists)]
          [t (cdr lists)])
      (if (null? t)
          (foldl (λ (e res) (cons (proc (reverse (cons e current))) res))
                 result h)
          (foldl (λ (e res) (for**/list-rec t proc (cons e current) res))
                 result h))))

  (if (null? lists)
      null
      (reverse (for**/list-rec lists proc '() '()))))


;; ----------------- Horn clause ----------------- ;;

(struct horn-clause (bound-vars premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port
              "∀(~a) [=> ~a ~a]"
              (string-join
               (set-map
                (horn-clause-bound-vars self)
                (curry format "~a"))
               ", ")
              (set->list (horn-clause-premises self))
              (horn-clause-conclusion self)))])

(define (horn-clause->implication clause)
  (match (horn-clause-premises clause)
    [(list) (horn-clause-conclusion clause)]
    [(list premise) (apply expression @=> (list premise (horn-clause-conclusion clause)))]
    [_ (expression @=>
                   (apply expression
                          `(, @&&
                            ,@(set->list (horn-clause-premises clause))))
                   (horn-clause-conclusion clause))]))

(define (rule->assertion clause)
  (dbg "Rule: ~a" clause)
  (cond
    [(horn-clause? clause)
     (replace-constants
      (common-vars-substitution (horn-clause-bound-vars clause))
      (horn-clause->implication clause))]
    [else clause]))

(define (enrich rules additional-bound-vars additional-premises)
  (for/list ([rule rules])
    (horn-clause (set-union additional-bound-vars (horn-clause-bound-vars rule))
                 (append additional-premises (horn-clause-premises rule))
                 (horn-clause-conclusion rule))))

(define-syntax rules->assertions
  (syntax-rules ()
    [(_ rules additional-bound-vars additional-premises) (map rule->assertion (enrich rules additional-bound-vars additional-premises))]
    [(_ additional-bound-vars additional-premises) (rules->assertions (current-rules) additional-bound-vars additional-premises)]))

(define (replace-constants subst t)
  (match t
    [(expression op args ...)
     (let ([args (map (curry replace-constants subst) args)])
       (apply expression `(,op ,@args)))]
    [(? relation?) t]
    [(constant _ _) (hash-ref subst t t)]
    [_ t]))

;; ----------------- Caches and parameters ----------------- ;;

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-bound-vars (make-parameter (set)))
(define current-premises (make-parameter (set)))
(define current-rules (make-parameter (list)))
(define current-intermediate-vars-count (make-parameter 0))
(define current-scope (make-parameter #f))
(define current-mutated-state (make-parameter #f))
(define current-mutations (make-parameter #f))
(define current-mutations-clauses (make-parameter #f))

(define common-bound-vars (make-parameter (make-hash)))
(define common-bound-vars-count (make-parameter 0))

(define (add-bound-var var)
  (current-bound-vars (set-add (current-bound-vars) var)))

(define (add-bound-vars vars)
  (current-bound-vars (set-union (current-bound-vars) vars)))

(define (add-premise premise)
  (current-premises (set-add (current-premises) premise)))

(define (add-premises premises)
  (current-premises (set-union (current-premises) premises)))

; Takes conclusion and constructs horn-clauses using current traversal context.
(define (add-rule value)
  (let* ([resulting-clauses
          (if (current-head)
              (for**/list (current-mutations-clauses)
                          (λ (clauses)
                            (let ([additional-bound-vars (apply set-union (map horn-clause-bound-vars clauses))]
                                  [additional-premises (apply set-union (map horn-clause-premises clauses))])
                              (horn-clause (set-union (current-bound-vars) additional-bound-vars)
                                           (set-union (current-premises) additional-premises)
                                           (function-application->relation (current-head)
                                                                           (map (λ (dep) (dep #t))
                                                                                (hash-ref read-dependencies (current-head)))
                                                                           (current-args)
                                                                           (map horn-clause-conclusion clauses)
                                                                           value)))))
              (list (horn-clause (current-bound-vars) (current-premises) value)))])
    (current-rules (append (current-rules) resulting-clauses))))

(define solvable-functions-cache (mutable-set))
(define relations-cache (make-hash))
(define auto-constants (make-hash))
(define read-dependencies (make-hash))
(define write-dependencies (make-hash))

(define (solvable-function? f)
  (set-member? solvable-functions-cache f))

(define (register-solvable-function f)
  (set-add! solvable-functions-cache f))

(define (function-application->symbolic-constant f args mutations)
  (let* ([id (fresh-intermediate-var 'ε)]
         [ε (constant id (solvable-range (type-of f)))]
         [read-deps (map (λ (dep) (dep #f)) (hash-ref read-dependencies f))]
         [write-deps mutations]
         [auto-premise (function-application->relation f read-deps args write-deps ε)]
         [auto-bound-vars (parameterize ([current-bound-vars (set)])
                            (eval/bound-vars auto-premise)
                            (current-bound-vars))]
         [auto-bound-vars-deps (apply set-union
                                      (for/list ([v auto-bound-vars])
                                        (if (hash-has-key? auto-constants v)
                                            (car (hash-ref auto-constants v ))
                                            (set))))]
         [auto-bound-vars (set-union auto-bound-vars auto-bound-vars-deps)]
         [auto-premise-deps (list->set
                             (apply set-union
                                    (for/list ([v auto-bound-vars])
                                      (if (hash-has-key? auto-constants v)
                                          (cdr (hash-ref auto-constants v))
                                          (set)))))]
         [auto-premises (set-add auto-premise-deps auto-premise)]
         [auto-value (cons auto-bound-vars auto-premises)])
    (for ([m mutations])
      (hash-set! auto-constants m auto-value))
    (hash-set! auto-constants ε auto-value)
    ε))

(define (function-application->relation f read-deps args write-deps ret)
  (let ([rel (hash-ref relations-cache f)])
    (apply expression `(, @app ,rel ,@read-deps ,@args ,@write-deps ,ret))))

(define bound-var-suffix "$<bound-var>")
(define relation-suffix "°")

(define (fresh-intermediate-var prefix)
  (begin0
    (format-id #f "~a~a" prefix (current-intermediate-vars-count))
    (current-intermediate-vars-count
     (add1 (current-intermediate-vars-count)))))

(define (fresh-bound-var id type)
  (constant (format-id #f "x!~a~a" (number->string (add1 id)) bound-var-suffix) type))

(define (fresh-relation f)
  (let* ([read-deps (filter global-var? (set->list (current-bound-vars)))]
         [write-deps (current-mutated-state)]
         [read-deps-types (map type-of read-deps)]
         [write-deps-types (map type-of (state->mutations write-deps))]
         [result (constant (string->symbol (format "~a~a" f relation-suffix))
                           (function-type->relational-type read-deps-types (type-of f) write-deps-types))])
    (hash-set! read-dependencies f (map encapsulate read-deps))
    (hash-set! write-dependencies f write-deps)
    result))

(define (bound-var? v)
  (and (constant? v) (string-suffix? (format "~a" v) bound-var-suffix)))

(define (relation? f)
  (and (constant? f) (string-suffix? (format "~a" f) relation-suffix) (equal? (solvable-range (type-of f)) @boolean?)))

(define (argument? v)
  (member v (current-args)))

(define (local-var? v)
  (and (current-scope) (set-member? (current-scope) v)))

(define (intermediate-var? v)
  (regexp-match #rx"^ε[0-9]+$" (format "~a" v)))

(define (global-var? v)
  (and (constant? v)
       (not (or (argument? v)
                (intermediate-var? v)
                (type-applicable? (type-of v))
                (local-var? v)))))

(define (encapsulate const)
  (λ (original?)
    (if original? const (symbolization->actual-value
                         (symbolization->actual-value const)))))

(define (fill-in-insufficient common-vars n type)
  (cond [(<= n (length common-vars)) common-vars]
        [else
         (let* ([insifficient-count (- n (length common-vars))]
                [insufficient-vars (map (λ (i) (fresh-bound-var (+ (common-bound-vars-count) i) type))
                                        (range insifficient-count))])
           (append common-vars insufficient-vars))]))

(define (common-vars-substitution bound-vars)
  (let* ([grouped-vars (group-by type-of (set->list bound-vars))]
         [grouped-vars (for/hash ([group grouped-vars]
                                  #:when (not (empty? group)))
                         (values (type-of (car group)) group))])
    ; First filling in insufficient bound vars...
    (for ([type (in-hash-keys grouped-vars)])
      (let* ([existing (hash-ref (common-bound-vars) type (list))]
             [filled (fill-in-insufficient existing
                                           (length (hash-ref grouped-vars type))
                                           type)]
             [added-count (- (length filled) (length existing))])
        (unless (<= added-count 0)
          (hash-set! (common-bound-vars) type filled)
          (common-bound-vars-count (+ (common-bound-vars-count) added-count)))))
    ; ... and then producing substitution map
    (make-hash
     (apply append
            (for/list ([type (in-hash-keys grouped-vars)])
              (for/list ([v1 (hash-ref grouped-vars type)]
                         [v2 (hash-ref (common-bound-vars) type)])
                (cons v1 v2)))))))

;; ----------------- Symbolic term -> Horn clauses ----------------- ;;

(define (eval/bound-vars t)
  (match t
    [(expression op args ...)
     (for ([arg args])
       (eval/bound-vars arg))]
    [(constant _ _) (add-bound-var t)]
    [_ (void)]))

(define (eval-body/horn t head args scope mutations)
  (parameterize ([current-head head]
                 [current-args args]
                 [current-scope scope]
                 [current-mutated-state mutations])
    (parameterize ([current-bound-vars (current-bound-vars)])
      (for ([m (state->mutations mutations)])
        (eval/bound-vars m))
      (eval/bound-vars t)
      (hash-set! relations-cache head (fresh-relation head)))

    (define mutations-clauses
      (parameterize ([current-head #f]
                     [current-args #f]
                     [current-scope #f]
                     [current-mutations (state->mutations (current-mutated-state))])
        (for/list ([m (current-mutations)])
          (term->rules m))))

    (parameterize ([current-mutations-clauses mutations-clauses])
      (eval/horn t))))

(define (eval/horn t)
  (parameterize ([current-intermediate-vars-count (current-intermediate-vars-count)]
                 [current-bound-vars (current-bound-vars)]
                 [current-premises (current-premises)])
    (term->horn-clauses #t t)))

(define (term->rules t)
  (parameterize ([current-rules (list)])
    (eval/horn t)
    (current-rules)))

(define (term->horn-clauses tail-position? t)
  (match t
    [(expression (== @app) f args ...)
     (let* ([args (terms->horn-clauses args)]
            [result
             (if (solvable-function? f)
                 (error 'rules (string-append "Some internal inconsistency: applications of "
                                              "procedures declared by define/unbound should not be met "
                                              "while encoding it. Please report this error!"))
                 (expression @app f args))])
       (when tail-position? (add-rule result))
       result)]
    [(expression (== ite) test then else)
     (let* ([test (term->horn-clauses #f test)]
            [then
             (parameterize ([current-premises (set-add (current-premises) test )])
               (term->horn-clauses tail-position? then))]
            [else
             (parameterize ([current-premises (set-add (current-premises) (@! test))])
               (term->horn-clauses tail-position? else))])
       (expression ite test then else))]
    [(expression (== ite*) gvs)
     (expression ite*
                 (map (λ (gv)
                        (match-let* ([(guarded g v) gv]
                                     [g (term->horn-clauses #f g)])
                          (parameterize ([current-premises (set-add (current-premises) g)])
                            (term->horn-clauses tail-position? v))))
                      gvs))]
    [(expression op args ...)
     (let* ([args (terms->horn-clauses args)]
            [result (apply expression `(,op ,@args))])
       (when tail-position? (add-rule result))
       result)]
    [(constant _ _)
     (add-bound-var t)
     (when (hash-has-key? auto-constants t)
       (match-define (cons auto-bound-vars auto-premises) (hash-ref auto-constants t))
       (add-bound-vars auto-bound-vars)
       (add-premises auto-premises))
     (when tail-position? (add-rule t))
     t]
    [_
     (when tail-position? (add-rule t))
     t]))

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

(define (function-type->relational-type read-dependencies t write-dependencies)
  (apply ~> `(,@read-dependencies ,@(solvable-domain t) ,@write-dependencies ,(solvable-range t) , @boolean?)))
