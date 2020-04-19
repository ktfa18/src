;-----------------------------
; Common Definition
;-----------------------------
; basic setting
(set-logic QF_SLIA)
(set-option :produce-models true)
(set-option :incremental true)

; Dummy constant for null 
(define-const null Int (- 99999))

;-----------------------------
; Select Statement
;-----------------------------
; From Clause:
(declare-const T2.C1 Int)
(declare-const T2.C2 Int)
(assert (or (and (>= T2.C1 (- 9)) (<= T2.C1 9)) (= T2.C1 null)))
(assert (or (and (>= T2.C2 (- 9)) (<= T2.C2 9)) (= T2.C2 null)))
(declare-const T1.C1 Int)
(declare-const T1.C2 Int)
(assert (and (and (>= T1.C1 (- 9)) (<= T1.C1 9)) (not (= T1.C1 null))))
(assert (or (and (and (>= T1.C2 (- 9)) (<= T1.C2 9)) (not (= T1.C2 0))) (= T1.C2 null)))
(assert (= T1.C1 T2.C1))

; Where Clause:
(assert (< T1.C2 T2.C2))

; Select Clause:
(declare-const F1 Int)
(assert (= F1 T1.C1))
(declare-const F2 Int)
(assert (= F2 (ite (< (+ T1.C2 T2.C2) 0) 0  (+ T1.C2 T2.C2))))

;-----------------------------
; Test Case
;-----------------------------
(echo "Case:1")
(push)
(assert (and (= F2 (+ T1.C2 T2.C2))(= F1 T1.C1)))
(check-sat)
(get-model)
(pop)

(echo "Case:2")
(push)
(assert (and (= F2 0)(= F1 T1.C1)))
(check-sat)
(get-model)
(pop)

