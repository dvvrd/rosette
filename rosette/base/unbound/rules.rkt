#lang racket

(require
  racket/syntax
  (only-in "../core/term.rkt"
           constant expression type-of @app
           solvable-domain solvable-range)
  (only-in "../core/polymorphic.rkt"
           ite ite* guarded)
  (only-in "../core/bool.rkt" @boolean? @forall @! @=> @&&)
  (only-in "../core/function.rkt" ~>))

(provide dbg @app (struct-out horn-clause)
         register-solvable-function
         rules->assertions term->rules eval/horn
         current-head current-args)

(define-syntax-rule (dbg ft args ...)
  (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port)))
;  void)


(struct horn-clause (free-vars premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port
              "∀(~a) [=> ~a ~a]\n"
              (string-join
               (map
                (curry format "~a")
                (horn-clause-free-vars self))
               ", ")
              (horn-clause-premises self)
              (horn-clause-conclusion self)))])

(define solvable-functions-cache (mutable-set))
(define relations-cache (make-hash))

(define (solvable-function? f)
  (set-member? solvable-functions-cache f))

(define (register-solvable-function f)
  (set-add! solvable-functions-cache f))

(define (function->relation f)
  (hash-ref! relations-cache f
             (thunk
              (constant (syntax->datum (format-id #f "~a°" (format "~a" f))) (function-type->relational-type (type-of f))))))

(define (eval/horn t)
  (term->horn-clauses #t t))

(define (term->rules t)
  (parameterize ([current-rules (list)])
    (term->horn-clauses #t t)
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
                   (add-free-var ε)
                   (add-premise (apply expression `(, @app ,rel ,@args ,ε)))
                   ε)
                 (expression @app f args))])
       (cond [tail-position? (add-rule result)])
       result)]
    [(expression (== ite) test then else)
     (let* ([test (term->horn-clauses #f test)]
            [then
             (parameterize ([current-premises (cons test (current-premises))]
                            [current-free-vars (current-free-vars)])
               (term->horn-clauses tail-position? then))]
            [else
             (parameterize ([current-premises (cons (@! test) (current-premises))]
                            [current-free-vars (current-free-vars)])
               (term->horn-clauses tail-position? else))])
       (expression ite test then else))]
    [(expression (== ite*) gvs)
     (expression ite*
                 (map (λ (gv)
                        (match-let* ([(guarded g v) gv]
                                     [g (term->horn-clauses #f g)])
                          (parameterize ([current-premises (cons g (current-premises))]
                                         [current-free-vars (current-free-vars)])
                            (term->horn-clauses tail-position? v))))
                      gvs))]
    [(expression op args ...)
     (let* ([args (terms->horn-clauses args)]
            [result (apply expression `(,op ,@args))])
       (cond [tail-position? (add-rule result)])
       result)]
    [(constant _ _)
     (add-free-var t)
     (cond [tail-position? (add-rule t)])
     t]
    [_
     (cond [tail-position? (add-rule t)])
     t]))

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

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
         [resulting-clause (horn-clause (current-free-vars) (current-premises) conclusion)])
    (unless (member resulting-clause (current-rules))
      (dbg "ADDING RULE ~a" resulting-clause)
      (current-rules (cons resulting-clause (current-rules))))))

(define (add-free-var var)
  (unless (member var (current-free-vars))
    (current-free-vars (cons var (current-free-vars)))))

(define (add-premise premise)
  (current-premises (cons premise (current-premises))))

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-free-vars (make-parameter (list)))
(define current-premises (make-parameter (list)))
(define current-rules (make-parameter (list)))

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
    [(_) (rules->assertions (current-rules))]))

; Transforms (@integer? ~> @integer?) to (@integer? ~> @integer? ~> @boolean?)
(define (function-type->relational-type t)
  (apply ~> `(,@(solvable-domain t) ,(solvable-range t) , @boolean?)))

(define current-intermediate-vars-count (make-parameter 0))

(define (fresh-intermediate-var prefix)
  (begin0
    (format-id #f "~a~a" prefix (current-intermediate-vars-count))
    (current-intermediate-vars-count
     (add1 (current-intermediate-vars-count)))))
