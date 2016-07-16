; http://github.com/seahorn/seahorn/blob/master/test/simple/03_array_unsat.c
; Correct answer: sat
; C-pseudocode:
;
; #define N 100
; // Different ways of initialization
; struct foo { int a; int b; int c;}; struct foo hh;
; int a[N];
; int x = 4;
; int b[3] = {2, 3, 4};
;
; int i;
; for (i = 0; i < N; ++i) {
;   if (rand())      a[i] = b[2];
;   else if (rand()) a[i] = hh.a;
;   else             a[i] = x;
; }
;
; int res = a[i-1];
; assert(res >= 0 && res <= 5);
;

(define-fun N () Int 100)
(declare-datatypes (T1 T2 T3) ((Tripple (mk-tripple (first T1) (second T2) (third T3)))))
(define-sort Foo () (Tripple Int Int Int))
(define-sort Arr () (Array Int Int))

(declare-var rand Bool)
(declare-var x Int)
(declare-var i Int)
(declare-var i1 Int)
(declare-var a Arr)
(declare-var a1 Arr)
(declare-var b Arr)
(declare-var hh Foo)
(declare-var res Int)
(declare-rel loop (Int Arr Arr Foo))
(declare-rel fail ())

(define-fun x-val () Bool (= x 4))
(define-fun b-val () Bool (= b (store (store (store b 2 4) 1 3) 0 2)))
(define-fun vals () Bool (and x-val b-val))
; Replacing previous line with next will make this test unsat
; (define-fun vals () Bool (and x-val b-val (= (first h) (second h) (third h) 3)))

(define-fun loop-condition () Bool (and (= i1 (+ i 1)) (< i1 N)))
(rule (=> (= i 0) (loop i a b hh)))
(rule (=> (and (loop i a b hh) vals rand (= a1 (store a i (select b 2))) loop-condition) (loop i1 a1 b hh)))
(rule (=> (and (loop i a b hh) vals rand (= a1 (store a i (first hh))) loop-condition) (loop i1 a1 b hh)))
(rule (=> (and (loop i a b hh) vals rand (= a1 (store a i x)) loop-condition) (loop i1 a1 b hh)))
(define-fun after-loop () Bool (and (not (< (+ i 1) N)) (loop i a b hh)))

(rule (=> (and after-loop (= res (select a (- i 1))) (not (and (>= res 0) (<= res 5)))) fail))
(query fail :print-certificate true)
