#lang racket

(require racket/splicing
         (only-in "../core/term.rkt" expression constant type-applicable?))

(provide (all-defined-out) gensym)

; Calls proc for all possible combinations of list-of-lists, returns a list
; of all results that those calls produced. If list-of-lists is empty then
; a list consisting of (proc '()) returned.
; TODO: is this just mapping cartesian-product? Test performance!
(define (for**/list list-of-lists proc)
  (define (for**/list-rec lists proc current result)
    (let ([h (car lists)]
          [t (cdr lists)])
      (if (null? t)
          (foldl (λ (e res) (cons (proc (reverse (cons e current))) res))
                 result h)
          (foldl (λ (e res) (for**/list-rec t proc (cons e current) res))
                 result h))))

  (if (null? list-of-lists)
      (list (proc '()))
      (reverse (for**/list-rec list-of-lists proc '() '()))))

(define-struct head-tail-stream (v)
    #:methods gen:stream
    [(define (stream-empty? stream)
       (empty? (head-tail-stream-v stream)))
     (define (stream-first stream)
       (values (first (head-tail-stream-v stream))
               (rest (head-tail-stream-v stream))))
     (define (stream-rest stream)
       (head-tail-stream (rest (head-tail-stream-v stream))))])

; Returns a sequence that returns (values (first xs) (rest xs)) for each element of xs.
(define (in-splits xs)
  (head-tail-stream xs))

; Returns a set of all symbolic constants of term t.
(define (term->constants t)
  (match t
    [(expression _ args ...) (terms->constants args)]
    [(constant _ type) (if (type-applicable? type) (set) (set t))]
    [(? list?) (terms->constants t)]
    [_ (set)]))

; Returns a set of all symbolic constants of terms ts.
(define (terms->constants ts)
  (foldl (λ (t acc)
           (set-union acc (term->constants t)))
         (set)
         ts))

; Returns set of values stored in hash1, but not stored in hash2 and
; filtered with filter-func.
(define (hash-values-diff+filter filter-func hash1 hash2)
  (let ([values1 (list->set (filter filter-func (hash-values hash1)))]
        [values2 (list->set (filter filter-func (hash-values hash2)))])
    (set-subtract values1 values2)))

; Replaces all occurences of keys-of-subst into term with corresponding values.
(define (substitute/constants subst t)
  (match t
    [(expression op args ...)
     (let ([new-args (map (curry substitute/constants subst) args)])
       (cond [(equal? args new-args) t]
             [else (apply expression `(,op ,@new-args))]))]
    [(constant _ _) (if (hash-has-key? subst t) (hash-ref subst t t) t)]
    [(list _ ...) (map (curry substitute/constants subst) t)]
    [_ t]))

; Returns two lists instead of list of pairs.
(define (unzip lst)
  (for/lists (l1 l2) ([pair lst])
    (values (car pair) (cdr pair))))

; Overloads racket gensym to simplify generated values. For each
; base it will return symbols starting from 1, thus identifiers
; will be more readable. Should be used only for convenience of debug!
; For releases this override should be return vanilla gensym values
; to exclude collision probability (definition can just be commented out for that).
(splicing-let ([gensym-cache (make-hash)])
  (define (gensym [base 'g])
    (let ([suffix (add1 (hash-ref gensym-cache base 0))])
      (hash-set! gensym-cache base suffix)
      (string->symbol (format "~a~a" base suffix)))))
