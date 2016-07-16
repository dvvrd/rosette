; Extended version of http://github.com/seahorn/seahorn/blob/master/test/simple/01-unsat.c
; Correct answer: unsat unsat
; C-pseudocode:
;
; int x = 1; int y = 1;
; while (unknown()) {
;   int t1 = x;  int t2 = y;
;   x = t1 + t2; y = t1 + t2;
; }
; assert(x == y);
; assert(y >= 1);
;

(declare-var x Int)
(declare-var x1 Int)
(declare-var y Int)
(declare-var y1 Int)
(declare-var t1 Int)
(declare-var t2 Int)
(declare-rel loop (Int Int))
(declare-rel fail1 ())
(declare-rel fail2 ())

(rule (=> (and (= x 1) (= y 1)) (loop x y)))
(rule (=> 
         (and (loop x y) (= t1 x) (= t2 y) (= x1 (+ t1 t2)) (= y1 (+ t1 t2)))
         (loop x1 y1)))

(rule (=> (and (loop x y) (not (= x y))) fail1))
(rule (=> (and (loop x y) (not (>= y 1))) fail2))

(query fail1 :print-certificate true)
(query fail2 :print-certificate true)
