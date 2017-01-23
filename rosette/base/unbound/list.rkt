#lang racket

(require
  (for-syntax racket/syntax (only-in "../core/lift.rkt" with@))
  racket/generic racket/provide racket/syntax
  "../core/term.rkt" "auto-constants.rkt" "define.rkt" "horn.rkt"
  (prefix-in bound/ "../adt/list.rkt")
  (only-in "../adt/seq.rkt" lift/apply/higher-order)
  (only-in "../form/control.rkt" @cond @if)
  (only-in "../core/bool.rkt" instance-of? || && @boolean?)
  (only-in "../core/equality.rkt" @eq? @equal?)
  (only-in "../core/function.rkt" ~>)
  (only-in "../core/lift.rkt" lift-id unsafe-merge**)
  (only-in "../core/merge.rkt" merge* unsafe-merge*)
  (only-in "../core/polymorphic.rkt" generic-merge*)
  (only-in "../core/real.rkt" @integer? @>= @= @< @+ @-)
  (only-in "../core/safe.rkt" assert assert-bound argument-error arguments-error assert-arity-includes assert-some)
  (only-in "../core/union.rkt" union in-union-guards union-guards union-filter union-contents)
  (only-in "dependencies.rkt" gen:implicitly-dependent)
  (only-in "lemmas.rkt" associative?)
  (only-in "relation.rkt" relation? fresh-relation))

(provide (except-out (filtered-out with@ (all-defined-out)) @list? @list)
         (rename-out [list/unbound? @list?] [list @list] [null @null]))


;; ----------------- Type declaration ----------------- ;;

; Represents an unbound list type.
(struct @list (element-type)
  #:transparent
  #:guard (lambda (elem caller)
            (unless (primitive-solvable? elem)
              (raise-argument-error caller "a primitive solvable type" elem))
            elem)
  #:property prop:procedure ; Recognizes lists of this type.
  (lambda (self v)
    (match v
      [(? list?) (andmap (@list-element-type self) v)]
      [(? pair?) (and ((@list-element-type self) (car v)) (self (cdr v)))]
      [(? typed? (app get-type (== self))) #t]
      [(union _ (or (== @list?) (== list?) (== @any/c)))
       (apply || (for/list ([g (in-union-guards v self)]) g))]
      [_ #f]))
  #:methods gen:type
  [(define (least-common-supertype self other)
     (match other
       [(or (== self) (== pair?) (== list?) (== bound/@pair?) (== bound/@list?)) self]
       [_ @any/c]))
   (define (type-name self) (string->symbol (~a self)))
   (define (type-applicable? self) #f)
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? typed? (app get-type (== self))) v]
       [(? pair?) v]
       [(? list?) v]
       [(union xs t)
        (cond [(subtype? t self) v]
              [(subtype? self t)
               (match (union-filter v self)
                 [(union (list (cons g u)) _)
                  (assert g (argument-error caller (~a list?) v))
                  u]
                 [u
                  (unless (= (length xs) (length (union-contents u)))
                    (assert (apply || (union-guards u)) (argument-error caller (~a list?) v)))
                  u])]
              [else (assert #f (argument-error caller (~a list?) v))])]
       [_ (assert #f (argument-error caller (~a list?) v))]))
   (define-list-comparator type-eq @eq?)
   (define-list-comparator type-equal? @equal?)
   (define (type-compress self force? ps) ps)
   (define (type-construct self vs)
     (cond
       [(null? vs) vs]
       [(equal? 'unbound-constant (car vs))
        (make-symbolic-list (second vs) (@list-element-type self) (third vs))]
       [else vs]))
   (define (type-deconstruct self v)
     (cond
       [(bound/@list? v) v]
       [else (list 'unbound-constant v (@length v))]))]
  #:methods gen:solvable
  [(define/generic generic-solvable-default solvable-default)
   (define (solvable-default self) null)
   (define (solvable-domain self) null)
   (define (solvable-range self) self)]
  #:methods gen:custom-write
  [(define (write-proc self port m)
     (fprintf port "listof ~a" (@list-element-type self)))]
  #:methods gen:implicitly-dependent
  [(define (implicit-dependencies self constant)
     (list (@length constant) (@car constant #f)))]
  #:methods gen:implicitly-constrained
  [(define (implicit-constraints self constant)
     (set (@>= (@length constant) 0)))])

(define-syntax-rule (define-list-comparator comparator =?)
  (define (comparator self u v)
    (unless (self u) (set!-values (u v) (values v u)))
    (match v
      [(? self) (@equal? (@length u) (@length v))]
      [(union gvs _)
       (generic-merge*
        (map
         (λ (gv)
           (cons (car gv) (@equal? u (cdr gv))))
         gvs))]
      [(? null?) (@null? u)]
      [(or (? pair?) (? list?) (? bound/@pair?) (? bound/@list?))
       (&& (=? (@length u) (@length v))
           (=? (@car u) (@car v))
           (=? (@cdr u) (@cdr v)))]
      [_ #f])))

;; ----------------- Storage of information about list constants ----------------- ;;

(define ids (make-hash))
(define lengths (make-hash))
(define real-values (make-hash))

(define-syntax (with-real-values stx)
  (syntax-case stx ()
    [(_ (lst ...) body body-rest ...)
     (quasisyntax/loc stx
       (let ()
         #,@(for/list ([l (syntax->list #'(lst ...))])
              (with-syntax ([updater (syntax-local-introduce (format-id #f "update-~a" l))])
                #`(define updater
                    (λ (val) (hash-set! real-values #,l val)))))
         (let ([lst (hash-ref real-values lst (thunk lst))] ...)
           body
           body-rest ...)))]))

(define-syntax-rule (define-list-processor (head args ...) body body-rest ...)
  (define (head args ...)
    (with-real-values (args ...)
      body
      body-rest ...)))

;; ----------------- Operators ----------------- ;;

; Getting f(x1 ... xn), g(y1 ... ym) and i constructs new typed function h such that:
; h(x1 ... x[i-1] y1 ... ym x[i+1] ... xn) = f(x1 ... x[i-1] g(y1 ... ym) x[i+1] ... xn).
; typeof xi must be same as range of g.
(define/contract (compose/typed f g i caller)
  (-> any/c any/c exact-nonnegative-integer? any/c any/c)
  (let* ([f-type (λtype f caller)]
         [f-domain (solvable-domain f-type)]
         [f-range (solvable-range f-type)]
         [g-type (λtype g caller)]
         [g-domain (solvable-domain g-type)]
         [g-range (solvable-range g-type)]
         [_ (assert-bound (i < (length f-domain)) caller)]
         [xi-type (list-ref f-domain i)])
    (assert (subtype? g-range xi-type) (error caller "~a is incompatible with ~a-th argument of ~a" g i f))
    (match* ((length f-domain) (length g-domain))
      [(1 0) (λ/typed () (~> f-range) (f (g)))]
      [(1 1) (λ/typed (x) (~> (car g-domain) f-range) (f (g x)))]
      [(2 1) (cond
               [(= i 0) (λ/typed (x y) (~> (car g-domain) (cadr f-domain) f-range) (f (g x) y))]
               [(= i 1) (λ/typed (x y) (~> (car f-domain) (car g-domain)  f-range) (f x (g y)))])]
      [(_ _) (error caller "Working with multiple lists is not supported yet")])))

(define-syntax-rule (λ-convert caller fun (args ...) type-convert body body-rest ...)
  (cond [(λtyped? fun)
         (λ/typed (args ...) (type-convert (λtype fun caller)) body body-rest ...)]
        [else
         (λ (args ...) body body-rest ...)]))

(define-operator mapped
  #:identifier 'mapped
  #:range (λ (proc lst)
            (solvable-range
             (λtype proc 'map)))
  #:unsafe map)

(define-operator appended
  #:identifier 'appended
  #:range (λ (xs ys)
            (@listof
             (least-common-supertype
              (@element-type xs)
              (@element-type ys))))
  #:unsafe append)

;; ----------------- Base list procedures ----------------- ;;

(define (@listof element-type)
  (@list element-type))

(define (@element-type lst)
  (match lst
    [(? null?) (raise-argument-error 'element-type "a non-empty list" lst)]
    [(? pair?) (type-of (car lst))]
    [(? list?) (type-of (car lst))]
    [(? @list?) (@list-element-type lst)]
    [(? list/unbound?) (@list-element-type (type-of lst))]
    [(union _ t) (@list-element-type t)]
    [_ (raise-argument-error 'element-type "list or list type" lst)]))

(define (list/unbound? v)
  (match v
    [(? list?) #t]
    [_ (@list? (type-of v))]))

(define (list/unbound/exactly? v)
  (@list? (type-of v)))

(define (@listof? t v)
  ((@listof t) v))

(define (make-symbolic-length name)
  (let ([result (constant (string->symbol (format "length-of-~a" name)) @integer?)])
    (register-auto-constant result (@>= result 0))
    result))

(define (make-symbolic-list id element-type [length #f])
  (let ([result (constant (string->symbol id) (@listof element-type))])
    (hash-set! lengths result (or length (make-symbolic-length id)))
    result))

(define (list-id constant)
  (hash-ref! ids constant (add1 (hash-count ids))))

(define-list-processor (@length lst)
  (match lst
    [(? null?) 0]
    [(? pair?) (@+ 1 (@length (cdr lst)))]
    [(expression (== mapped) _ other-lst)
     (with-real-values (other-lst)
       (@length other-lst))]
    [(expression (== appended) xs ys)
     (with-real-values (xs ys)
       (@+ (@length xs) (@length ys)))]
    [(? list/unbound/exactly?) (hash-ref! lengths lst (thunk (make-symbolic-length lst)))]
    [(union vs)
     (apply merge*
            (for/list ([v vs])
              (cons (car v) (@length (cdr v)))))]))

(define (@null? lst)
  (@= (@length lst) 0))

(define-syntax (define-list-decomposer stx)
  (syntax-case stx ()
    [(_ id #:mapper mapper #:appender appender)
     (with-syntax ([lifted-id (lift-id #'id)])
       (quasisyntax/loc stx
         (define (lifted-id lst [update? #t])
           (with-real-values (lst)
             (match lst
               [(? list?) (id lst)]
               [(? pair?) (id lst)]
               [(expression (== mapped) proc other-lst)
                (with-real-values (other-lst)
                  (mapper proc other-lst))]
               [(expression (== appended) xs ys)
                (with-real-values (xs ys)
                  (appender xs ys))]
               [(? list/unbound?)
                (let* ([l (@length lst)]
                       [fresh-head (constant (string->symbol (format "head-of-~a" lst))
                                             (@element-type lst))]
                       [fresh-tail (make-symbolic-list (format "tail-of-~a" lst)
                                                       (@element-type lst)
                                                       (@- l 1))]
                       [fresh-list (cons fresh-head fresh-tail)])
                  (when update?
                    ;(assert-bound (0 @< l) 'id)
                    (update-lst fresh-list))
                  (id fresh-list))]
               [(union vs)
                (apply merge*
                       (for/list ([v vs])
                         (cons (car v) (lifted-id (cdr v)))))])))))]))

(define-list-decomposer car
  #:mapper (λ (proc xs) (proc (@car xs)))
  #:appender (λ (xs ys) (@if (@null? xs) (@car ys) (@car xs))))

(define-list-decomposer cdr
  #:mapper (λ (proc xs) (@map proc (@cdr xs)))
  #:appender (λ (xs ys) (@if (@null? xs) (@cdr ys) (@append (@cdr xs) ys))))

(define-list-processor (append/unsafe xs ys)
  (match* (xs ys)
    [((? null?) _) ys]
    [(_ (? null?)) xs]
    [((? pair?) _) (cons (car xs) (append/unsafe (cdr xs) ys))]
    [((expression (== mapped) f vs) (expression mapped g ws)) #:when (eq? f g)
     (with-real-values (vs ws)
       (@map f (append/unsafe vs ws)))]
    [((union vs) (union ws))
     (apply unsafe-merge*
            (assert-some
             (for*/list ([v vs] [w ws] [g (in-value (&& (car v) (car w)))] #:when g)
               (cons g (append/unsafe (cdr v) (cdr w))))
             #:unless (* (length vs) (length ws))
             (arguments-error 'append (format "expected ~a ~a" list? list?)
                              "first argument" vs "second argument" ws)))]
    [((union vs) _) (unsafe-merge** vs (append/unsafe xs _))]
    [(_ (union vs)) (unsafe-merge** vs (append/unsafe _ ys))]
    [(_ _) (expression appended xs ys)]))

(define @append
  ; TODO: typecasts are needed here
  (case-lambda
    [()      null]
    [(xs)    xs]
    [(xs ys) (append/unsafe xs ys)]
    [xss     (for/fold ([out (list)])
                       ([xs xss])
               (append/unsafe out xs))]))

;; ----------------- Iterators ----------------- ;;

(define-syntax-rule (apply/union/higher-order iterator lifted proc init ... lst)
  (let* ([unbound? (λtyped? proc)]
         [@list? (if unbound?
                     (@listof (car
                               (solvable-domain
                                (λtype proc 'iterator))))
                     bound/@list?)])
    (lift/apply/higher-order lifted proc init ... lst : 'iterator : list? -> @list?)))

(define-list-processor (@foldl proc init lst)
  (define (fold/unbound init lst)
    (let* ([proc-type (λtype proc 'foldl)]
           [domain (solvable-domain proc-type)]
           [range (solvable-range proc-type)])
      (assert-arity-includes proc 2 'foldl)
      (unless (equal? (cadr domain) range)
        (argument-error 'foldl (format "proc of type ~a -> ~a -> ~a" (car domain) range range) proc-type))
      (define/unbound/higher foldl () (init lst) (~> range (@listof (car domain)) range)
        (@cond
         [(@null? lst) init]
         [else (foldl
                (proc (@car lst #f) init)
                (@cdr lst #f))]))
      (foldl init lst)))

  (match lst
    [(? null?) init]
    [(? pair?) (@foldl proc (proc (car lst) init) (cdr lst))]
    [(expression (== mapped) other-proc other-lst)
     (with-real-values (other-lst)
       (@foldl (compose/typed proc other-proc 0 'foldl) init other-lst))]
    [(expression (== appended) xs ys)
     (if (associative? proc)
         (@if #f;(@null? ys)
              (@foldl proc init xs)
              (proc (@foldl proc init xs) (@foldl proc (@car ys) (@cdr ys))))
         (@if #f;(@null? xs)
              (@foldl proc init ys)
              (@foldl proc (@foldl proc (@car xs) (@cdr xs)) ys)))]
    [(? list/unbound/exactly?) (fold/unbound init lst)]
;     (let ([result (fold/unbound init lst)])
;       (deferred-merge lst result)
;       result)]
    [(union _) (apply/union/higher-order foldl @foldl proc init lst)]
    [_ (raise-argument-error 'foldl "list" lst)]))


(define-list-processor (@map proc lst)
  (match lst
    [(? null?) null]
    [(? pair?) (cons (proc (car lst)) (@map proc (cdr lst)))]
    [(expression (== mapped) other-proc other-lst)
     (with-real-values (other-lst)
       (@map (compose/typed proc other-proc 0 'map) other-lst))]
    [(expression (== appended) xs ys) (expression mapped proc lst)]
    [(? list/unbound/exactly?) (expression mapped proc lst)]
    [(union _) (apply/union/higher-order map @map proc lst)]
    [_ (raise-argument-error 'map "list" lst)]))

(define-syntax (define-op-map stx)
  (syntax-case stx ()
    [(_ lifted-op op default)
     (with-syntax ([id (format-id stx "@~amap" (syntax->datum #'op))])
       (quasisyntax/loc stx
         (define (id pred lst)
           (@foldl (λ-convert 'id pred
                              (elem acc)
                              (λ (t) (apply ~> `(,@(solvable-domain t) , @boolean? , @boolean?)))
                              (lifted-op acc (pred elem)))
                   default
                   lst))))]))

(define-op-map && and #t)
(define-op-map || or  #f)

;; ----------------- Elimination from solver encoding ----------------- ;;

(struct symbolic-lists-eliminator ()
  #:methods gen:horn-transformer
  [(define (pre-process self clauses)
     (parameterize ([current-relations-subst (make-hash)])
       (eliminate* clauses)))
   (define (post-process self terms)
     terms)])

(register-horn-transformer (symbolic-lists-eliminator))

(define current-relations-subst (make-parameter #f))

(define (eliminate* clauses)
  (define keys (hash-keys clauses))
  (for ([key (in-list keys)])
    (let ([eliminated (map eliminate/clause (hash-ref clauses key))]
          [rel? (relation? key)])
      (when rel?
        (hash-remove! clauses key))
      (hash-set! clauses (if rel? (eliminate/relation key) key) eliminated))))

(define (eliminate/clause clause)
  (let ([conclusion (eliminate/term (horn-clause-conclusion clause))]
        [premises (set-map (horn-clause-premises clause) eliminate/term)])
    (horn-clause premises conclusion)))

(define (eliminate/term term)
  (match term
    [(? relation?) (eliminate/relation term)]
    [(constant _ (? @list?)) (@length term)]
    [(expression op args ...)
     (let ([new-args (map eliminate/term args)])
       (if (equal? new-args args)
           term
           (apply expression `(,op ,@new-args))))]
    [(list _ ...) (@length term)]
    [(cons _ _) (@length term)]
    [_ term]))

(define (eliminate/relation rel)
  (hash-ref! (current-relations-subst) rel
             (thunk
              (let* ([type (type-of rel)]
                     [old-domain (solvable-domain type)]
                     [new-domain (map (λ (type)
                                        (if (@list? type)
                                            @integer?
                                            type))
                                      old-domain)])
                (cond
                  [(equal? new-domain old-domain) rel]
                  [else
                   (fresh-relation (string->symbol (format "w/list:~a" rel))
                                   '() new-domain '() '())])))))


;; ----------------- Pair and List Accessor Shorthands ----------------- ;;

(define (@caar x) (@car (@car x)))
(define (@cdar x) (@cdr (@car x)))
(define (@cadr x) (@car (@cdr x)))
(define (@cddr x) (@cdr (@cdr x)))
(define (@caaar x) (@car (@car (@car x))))
(define (@cdaar x) (@cdr (@car (@car x))))
(define (@caadr x) (@car (@car (@cdr x))))
(define (@cdadr x) (@cdr (@car (@cdr x))))
(define (@cadar x) (@car (@cdr (@car x))))
(define (@cddar x) (@cdr (@cdr (@car x))))
(define (@caddr x) (@car (@cdr (@cdr x))))
(define (@cdddr x) (@cdr (@cdr (@cdr x))))
(define (@caaaar x) (@car (@car (@car (@car x)))))
(define (@cdaaar x) (@cdr (@car (@car (@car x)))))
(define (@caaadr x) (@car (@car (@car (@cdr x)))))
(define (@cdaadr x) (@cdr (@car (@car (@cdr x)))))
(define (@caadar x) (@car (@car (@cdr (@car x)))))
(define (@cdadar x) (@cdr (@car (@cdr (@car x)))))
(define (@caaddr x) (@car (@car (@cdr (@cdr x)))))
(define (@cdaddr x) (@cdr (@car (@cdr (@cdr x)))))
(define (@cadaar x) (@car (@cdr (@car (@car x)))))
(define (@cddaar x) (@cdr (@cdr (@car (@car x)))))
(define (@cadadr x) (@car (@cdr (@car (@cdr x)))))
(define (@cddadr x) (@cdr (@cdr (@car (@cdr x)))))
(define (@caddar x) (@car (@cdr (@cdr (@car x)))))
(define (@cdddar x) (@cdr (@cdr (@cdr (@car x)))))
(define (@cadddr x) (@car (@cdr (@cdr (@cdr x)))))
(define (@cddddr x) (@cdr (@cdr (@cdr (@cdr x)))))

; TODO: ensure that lst is list
(define (@first lst) (@car lst))
(define (@second lst) (@cadr lst))
(define (@third lst) (@caddr lst))
(define (@fourth lst) (@cadddr lst))
(define (@fifth lst) (@car (@cadddr lst)))
(define (@sixth lst) (@cadr (@cadddr lst)))
(define (@seventh lst) (@caddr (@cadddr lst)))
(define (@eighth lst) (@cadddr (@cadddr lst)))
(define (@ninth lst) (@car (@cadddr (@cadddr lst))))
(define (@tenth lst) (@cadr (@cadddr (@cadddr lst))))
