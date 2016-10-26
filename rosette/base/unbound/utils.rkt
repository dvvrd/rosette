#lang racket

(require (only-in "../core/term.rkt" expression constant))

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
    [(constant _ _) (set t)]
    [_ (set)]))

; Returns a set of all symbolic constants of terms ts.
(define (terms->constants ts)
  (foldl (λ (t acc)
           (set-union acc (term->constants t)))
         (set)
         ts))
