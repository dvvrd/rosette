#lang racket

(require
  racket/syntax
  (only-in "../core/term.rkt"
           define-operator constant expression
           type-of @app solvable-domain solvable-range)
  (only-in "../core/polymorphic.rkt"
           ite ite* guarded)
  (only-in "../core/bool.rkt" @boolean? @! @=> @&&)
  (only-in "../core/function.rkt" ~>))

(provide dbg @app @rel @var @rule
         (struct-out horn-clause)
         register-solvable-function
         rules->assertions term->rules eval/horn
         current-head current-args)

(define-syntax-rule (dbg ft args ...)
  (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port)))
;  void)

;; ----------------- Horn clause and operators ----------------- ;;

(struct horn-clause (bound-vars premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port
              "∀(~a) [=> ~a ~a]\n"
              (string-join
               (map
                (curry format "~a")
                (horn-clause-bound-vars self))
               ", ")
              (horn-clause-premises self)
              (horn-clause-conclusion self)))])

; Defines an operator that simply does what its only argument does.
; It is useful for bringing some more info to datalog format printer.
; Operators declared by this macro intended only for internal use
; and should not be called by client code.
(define-syntax (define-wrapper-operator stx)
  (with-syntax ([(_ id) stx])
    #'(define-operator id
        #:identifier 'id
        #:range (λ (expr) (type-of expr))
        #:unsafe (λ (expr) expr))))

(define-wrapper-operator @rel)
(define-wrapper-operator @var)
(define-wrapper-operator @rule)

;; ----------------- Caches and parameters ----------------- ;;

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-bound-vars (make-parameter (set)))
(define current-premises (make-parameter (list)))
(define current-rules (make-parameter (list)))

(define (add-bound-var var)
  (current-bound-vars (set-add (current-bound-vars) var)))

(define (add-premise premise)
  (current-premises (cons premise (current-premises))))

; Takes conclusion and constructs horn-clause using current traversal context.
(define (add-rule value)
  (let* ([conclusion
          (if (current-head)
              (apply expression
                     `(, @app
                       , (function->relation (current-head))
                       ,@(current-args)
                       ,value))
              value)]
         [resulting-clause (horn-clause (current-bound-vars) (current-premises) conclusion)])
    (unless (member resulting-clause (current-rules))
      (current-rules (cons resulting-clause (current-rules))))))

(define solvable-functions-cache (mutable-set))
(define relations-cache (make-hash))

(define (solvable-function? f)
  (set-member? solvable-functions-cache f))

(define (register-solvable-function f)
  (set-add! solvable-functions-cache f))

(define (function->relation f)
  (hash-ref! relations-cache f
             (thunk
              (expression @rel
                          (constant (syntax->datum (format-id #f "~a°" (format "~a" f)))
                                    (function-type->relational-type (type-of f)))))))

(define current-intermediate-vars-count (make-parameter 0))

(define (fresh-intermediate-var prefix)
  (begin0
    (format-id #f "~a~a" prefix (current-intermediate-vars-count))
    (current-intermediate-vars-count
     (add1 (current-intermediate-vars-count)))))

(define common-bound-vars (make-parameter (make-hash)))
(define common-bound-vars-count (make-parameter 0))

(define (fresh-bound-var id type)
  (expression @var (constant (format-id #f "x!~a" (number->string (add1 id))) type)))

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
                 (let* ([id (fresh-intermediate-var 'ε)]
                        [ε (constant id (solvable-range (type-of f)))]
                        [rel (function->relation f)])
                   (add-bound-var ε)
                   (add-premise (apply expression `(, @app ,rel ,@args ,ε)))
                   ε)
                 (expression @app f args))])
       (cond [tail-position? (add-rule result)])
       result)]
    [(expression (== ite) test then else)
     (let* ([test (term->horn-clauses #f test)]
            [then
             (parameterize ([current-premises (cons test (current-premises))]
                            [current-bound-vars (current-bound-vars)])
               (term->horn-clauses tail-position? then))]
            [else
             (parameterize ([current-premises (cons (@! test) (current-premises))]
                            [current-bound-vars (current-bound-vars)])
               (term->horn-clauses tail-position? else))])
       (expression ite test then else))]
    [(expression (== ite*) gvs)
     (expression ite*
                 (map (λ (gv)
                        (match-let* ([(guarded g v) gv]
                                     [g (term->horn-clauses #f g)])
                          (parameterize ([current-premises (cons g (current-premises))]
                                         [current-bound-vars (current-bound-vars)])
                            (term->horn-clauses tail-position? v))))
                      gvs))]
    [(expression op args ...)
     (let* ([args (terms->horn-clauses args)]
            [result (apply expression `(,op ,@args))])
       (cond [tail-position? (add-rule result)])
       result)]
    [(constant _ _)
     (add-bound-var t)
     (cond [tail-position? (add-rule t)])
     t]
    [_
     (cond [tail-position? (add-rule t)])
     t]))

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

; Transforms (@integer? ~> @integer?) to (@integer? ~> @integer? ~> @boolean?)
(define (function-type->relational-type t)
  (apply ~> `(,@(solvable-domain t) ,(solvable-range t) , @boolean?)))

;; ----------------- Post-processing ----------------- ;;

(define (horn-clause->implication clause)
  (if (empty? (horn-clause-premises clause))
      (horn-clause-conclusion clause)
      (expression @=>
                  (apply expression
                         `(, @&&
                           ,@(flatten (horn-clause-premises clause))))
                  (horn-clause-conclusion clause))))

(define (rule->assertion clause)
  (if (horn-clause? clause)
      (replace-constants
       (common-vars-substitution (horn-clause-bound-vars clause))
       (expression @rule (horn-clause->implication clause)))
      clause))

(define (replace-constants subst t)
  (match t
    [(expression (== @rel) f) t]
    [(expression op args ...)
     (let ([args (map (curry replace-constants subst) args)])
       (apply expression `(,op ,@args)))]
    [(constant _ _) (hash-ref subst t t)]
    [_ t]))

(define-syntax rules->assertions
  (syntax-rules ()
    [(_ rules) (map rule->assertion rules)]
    [(_) (rules->assertions (current-rules))]))
