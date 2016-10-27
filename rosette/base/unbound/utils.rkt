#lang racket

(require (only-in "../core/term.rkt" expression constant type-applicable?))

(provide (all-defined-out))

; Calls proc for all possible combinations of list-of-lists, returns a list
; of all results that those calls produced. If list-of-lists is empty then
; a list consisting of (proc '()) returned.
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

; Returns a set of all symbolic constants of term t.
(define (term->constants t)
  (match t
    [(expression op args ...) (terms->constants args)]
    [(constant _ type) (if (type-applicable? type) (set) (set t))]
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
