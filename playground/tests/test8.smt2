; http://github.com/seahorn/seahorn/blob/master/test/simple/05_recursive_sat.c
; Correct answer: sat
; C-pseudocode:
;
; int id(int x) {
;   if (x == 0) return 0;
;   int ret = id2(x-1) + 1;
;   if (ret > 2) return 2;
;   return ret;
; }
;
; int id2(int x) {
;   if (x==0) return 0;
;   int ret = id(x-1) + 1;
;   if (ret > 2) return 2;
;   return ret;
; }
;
; int main(void) {
;   int result = id(rand());
;   assert(result == 3);
; }
;

(declare-rel id (Int Int))
(declare-rel id2 (Int Int))
(declare-var x Int)
(declare-var t1 Int)
(declare-var ret Int)
(declare-rel fail ())

(define-fun middle () Bool (and (not (= x 0)) (id2 (- x 1) t1) (= ret (+ t1 1))))
(rule (=> (= x 0) (id x 0)))
(rule (=> (and middle (> ret 2)) (id x 2)))
(rule (=> (and middle (not (> ret 2))) (id x ret)))

(define-fun middle2 () Bool (and (not (= x 0)) (id (- x 1) t1) (= ret (+ t1 1))))
(rule (=> (= x 0) (id2 x 0)))
(rule (=> (and middle2 (> ret 2)) (id2 x 2)))
(rule (=> (and middle2 (not (> ret 2))) (id2 x ret)))

(rule (=> (and (id x ret) (not (= x ret))) fail))

(query fail :print-certificate true)
