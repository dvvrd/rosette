; http://github.com/seahorn/seahorn/blob/master/test/simple/02_array_sat.c
; Correct answer: unsat
; C-pseudocode:
;
; #define N 10
; int a[N];
; int i;
; for (i = 0; i < N; ++i) a[i] = 0;
; for (i = 0; i < N; ++i) assert(a[i] == 0);
;

(define-fun N () Int 10)
(declare-var a (Array Int Int))
(declare-var a1 (Array Int Int))
(declare-var i Int)
(declare-var i1 Int)
(declare-var j Int)
(declare-var j1 Int)
(declare-rel loop1 (Int (Array Int Int)))
(declare-rel loop2 (Int (Array Int Int)))
(declare-rel fail ())

(define-fun loop1-condition () Bool (and (= i1 (+ i 1)) (< i1 N)))
(rule (=> (= i 0) (loop1 i a)))
(rule (=> (and (loop1 i a) (= a1 (store a i 0)) loop1-condition) (loop1 i1 a1)))
(define-fun after-loop1 () Bool (and (not (< (+ i 1) N)) (loop1 i a)))

(define-fun loop2-condition () Bool (and (= j1 (+ j 1)) (< j1 N)))
(rule (=> (and after-loop1 (= j 0)) (loop2 j a)))
(rule (=> (and after-loop1 (loop2 j a) (not (= (select a j) 0)) loop2-condition) fail))
(rule (=> (and after-loop1 (loop2 j a) (= (select a j) 0) loop2-condition) (loop2 j a)))

(query fail :print-certificate true)
