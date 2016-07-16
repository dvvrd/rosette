; http://github.com/seahorn/seahorn/blob/master/test/simple/04_recursive_sat.c
; Correct answer: unsat
; C-pseudocode:
;
; int fibo(int n) {
;     if (n < 1) return 0;
;     else if (n == 1) return 1;
;     else return fibo(n-1) + fibo(n-2);
; }
;
; // fibo 1-30
; // 1, 1, 2, 3, 5,
; // 8, 13, 21, 34, 55, 
; // 89, 144, 233, 377, 610,
; // 987, 1597, 2584, 4181, 6765,
; // 10946, 17711, 28657, 46368, 75025,
; // 121393, 196418, 317811, 514229, 832040
;
; int x = 10;
; int result = fibo(x);
; assert(result == 55);
;

(declare-rel fib (Int Int))
(declare-var n Int)
(declare-var t1 Int)
(declare-var t2 Int)
(declare-rel fail ())

(rule (=> (< n 1) (fib n 0)))
(rule (=> (= n 1) (fib n 1)))
(rule (=> (and (not (< n 1)) (not (= n 1)) (fib (- n 1) t1) (fib (- n 2) t2)) (fib n (+ t1 t2))))

(rule (=> (and (= n 10) (fib n t1) (not (= t1 55))) fail))
(query fail :print-certificate true)
