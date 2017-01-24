#lang rosette/unbound

(dbg-level 0)
(define-symbolic b2 b3 integer?)

(define/unbound (loop x) (~> integer? integer?)
  (loop x))

(define init 0)
(define opened 1)
(define closed 2)
(define ignore 3)

(define (readit st)
  (cond
    [(= st opened) opened]
    [(= st ignore) st]
    [else (assert false)]))

(define (read_ x st)
  (if x (readit st) st))

(define (closeit st)
  (cond
    [(= st opened) closed]
    [(= st ignore) st]
    [else (loop) 0]))

(define (close_ x st)
  (if x (closeit st) st))

(define/unbound (f x y st) (~> boolean? boolean? integer? integer?)
  (close_ y (close_ x st))
  (f x y (read_ y (read_ x st)))
  1)

(define (next st)
  (if (= st init) opened ignore))

(define (g b3 x st)
  (if (> b3 0)
      (f x #t (next st))
      (f x #f st)))

; Expecting unsat
(time
 (verify/unbound
  (assert
   (positive?
    (if (> b2 0)
        (g b3 #t opened)
        (g b3 #f init))))))
