#lang racket

(require
  racket/syntax
  (only-in "../core/term.rkt"
           define-operator constant constant? expression
           type-of @app solvable-domain solvable-range type-applicable?)
  (only-in "../core/polymorphic.rkt"
           ite ite* guarded)
  (only-in "../core/bool.rkt" @boolean? @! @=> @&&)
  (only-in "../core/function.rkt" ~>))

(provide dbg @app (struct-out horn-clause)
         register-solvable-function
         rules->assertions term->rules eval/horn
         current-head current-args current-scope
         bound-var? relation?)

(define-syntax-rule (dbg ft args ...)
  (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port)))
;  void)

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
              (horn-clause-premises self)
              (horn-clause-conclusion self)))])

(define (horn-clause->implication clause)
  (match (horn-clause-premises clause)
    [(list) (horn-clause-conclusion clause)]
    [(list premise) (apply expression @=> (list premise (horn-clause-conclusion clause)))]
    [_ (expression @=>
                   (apply expression
                          `(, @&&
                            ,@(flatten (horn-clause-premises clause))))
                   (horn-clause-conclusion clause))]))

(define (rule->assertion clause)
  (dbg "Rule: ~a" clause)
  (if (horn-clause? clause)
      (replace-constants
       (common-vars-substitution (horn-clause-bound-vars clause))
       (horn-clause->implication clause))
      clause))

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
(define current-premises (make-parameter (list)))
(define current-rules (make-parameter (list)))
(define current-intermediate-vars-count (make-parameter 0))
(define current-scope (make-parameter (set)))

(define common-bound-vars (make-parameter (make-hash)))
(define common-bound-vars-count (make-parameter 0))

(define (add-bound-var var)
  (current-bound-vars (set-add (current-bound-vars) var)))

(define (add-premise premise)
  (current-premises (cons premise (current-premises))))

; Takes conclusion and constructs horn-clause using current traversal context.
(define (add-rule value)
  (let* ([conclusion (if (current-head)
                         (function-application->relation (current-head) (current-args) value)
                         value)]
         [resulting-clause (horn-clause (current-bound-vars) (current-premises) conclusion)])
    (unless (member resulting-clause (current-rules))
      (current-rules (cons resulting-clause (current-rules))))))

(define solvable-functions-cache (mutable-set))
(define relations-cache (make-hash))
(define global-state-dependencies (make-hash))

(define (solvable-function? f)
  (set-member? solvable-functions-cache f))

(define (register-solvable-function f)
  (set-add! solvable-functions-cache f))

(define (function-application->relation f args ret)
  (let* ([rel (hash-ref! relations-cache f
                        (thunk (fresh-relation f)))]
         [global-dependencies (hash-ref global-state-dependencies rel)])
    (apply expression `(, @app ,rel ,@global-dependencies ,@args ,ret))))

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
  (let* ([global-dependencies (filter global-var? (set->list (current-bound-vars)))]
         [domain-extension (map type-of global-dependencies)]
         [result (constant (string->symbol (format "~a~a" f relation-suffix))
                           (function-type->relational-type domain-extension (type-of f)))])
    (hash-set! global-state-dependencies result global-dependencies)
    result))

(define (bound-var? v)
  (and (constant? v) (string-suffix? (format "~a" v) bound-var-suffix)))

(define (relation? f)
  (and (constant? f) (string-suffix? (format "~a" f) relation-suffix) (equal? (solvable-range (type-of f)) @boolean?)))

(define (argument? v)
  (member v (current-args)))

(define (local-var? v)
  (set-member? (current-scope) v))

(define (intermediate-var? v)
  (regexp-match #rx"^ε[0-9]+$" (format "~a" v)))

(define (global-var? v)
  (and (constant? v)
       (not (or (argument? v)
                (intermediate-var? v)
                (type-applicable? (type-of v))
                (local-var? v)))))

(define (fill-in-insufficient common-vars n type)
  (if (<= n (length common-vars))
      common-vars
      (let* ([insifficient-count (- n (length common-vars))]
             [insufficient-vars (map (λ (i)
                                       (fresh-bound-var (+ (common-bound-vars-count) i) type))
                                     (range insifficient-count))])
        (append common-vars insufficient-vars))))

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

(define (eval/horn t)
  (parameterize ([current-intermediate-vars-count (current-intermediate-vars-count)]
                 [current-bound-vars (current-bound-vars)]
                 [current-premises (current-premises)])
    (eval/bound-vars t)
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
                 (let* ([id (fresh-intermediate-var 'ε)]
                        [ε (constant id (solvable-range (type-of f)))])
                   (add-bound-var ε)
                   (add-premise (function-application->relation f args ε))
                   ε)
                 (expression @app f args))])
       (cond [tail-position? (add-rule result)])
       result)]
    [(expression (== ite) test then else)
     (let* ([test (term->horn-clauses #f test)]
            [then
             (parameterize ([current-premises (cons test (current-premises))])
               (term->horn-clauses tail-position? then))]
            [else
             (parameterize ([current-premises (cons (@! test) (current-premises))])
               (term->horn-clauses tail-position? else))])
       (expression ite test then else))]
    [(expression (== ite*) gvs)
     (expression ite*
                 (map (λ (gv)
                        (match-let* ([(guarded g v) gv]
                                     [g (term->horn-clauses #f g)])
                          (parameterize ([current-premises (cons g (current-premises))])
                            (term->horn-clauses tail-position? v))))
                      gvs))]
    [(expression op args ...)
     (let* ([args (terms->horn-clauses args)]
            [result (apply expression `(,op ,@args))])
       (cond [tail-position? (add-rule result)])
       result)]
    [(constant _ _)
     (cond [tail-position? (add-rule t)])
     t]
    [_
     (cond [tail-position? (add-rule t)])
     t]))

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

; Transforms (@integer? ~> @integer?) to (@integer? ~> @integer? ~> @boolean?)
(define (function-type->relational-type additional-domain t)
  (apply ~> `(,@additional-domain ,@(solvable-domain t) ,(solvable-range t) , @boolean?)))
