;-----------------------------
; Common Definition
;-----------------------------
; basic setting
(set-option :produce-proofs true)
(set-option :pp.bv-literals false)
(set-logic QF_BV)

; Dummy constant for null
(define-const null (_ BitVec 32) (bvneg (_ bv99 32)))

;-----------------------------
; Select Statement
;-----------------------------
; From Clause:
(declare-const L.L1 (_ BitVec 32))
(assert (and (and (bvsge L.L1 (bvneg (_ bv9 32))) (bvsle L.L1 (_ bv9 32)) ) (not (= L.L1 null))))
(assert (bvugt L.L1 (_ bv0 32)))
(declare-const S.S1 (_ BitVec 32))
(declare-const S.S2 (_ BitVec 32))
(declare-const S.S3 (_ BitVec 32))
(assert (and (and (bvsge S.S1 (bvneg (_ bv9 32))) (bvsle S.S1 (_ bv9 32)) ) (not (= S.S1 null))))
(assert (and (and (bvsge S.S2 (bvneg (_ bv9 32))) (bvsle S.S2 (_ bv9 32)) ) (not (= S.S2 null))))
(assert (and (and (bvsge S.S3 (bvneg (_ bv9 32))) (bvsle S.S3 (_ bv9 32)) ) (not (= S.S3 null))))

; Where Clause:
(assert (bvugt S.S1 (_ bv0 32)))

; Select Clause:
(declare-const COL1 (_ BitVec 32))
(assert (= COL1 (ite (= S.S1 (_ bv1 32)) (ite (= L.L1 (_ bv2 32)) (_ bv111 32)  (_ bv222 32)) (ite (= S.S1 (_ bv2 32)) (_ bv333 32)  (_ bv444 32)))))

;-----------------------------
; Path Pattern and Get Model
;-----------------------------
(echo "Path:1")
(push)
(assert (and (= COL1 (_ bv444 32))))
(check-sat)
(get-model)
(pop)

(echo "Path:2")
(push)
(assert (and (= COL1 (_ bv222 32))))
(check-sat)
(get-model)
(pop)

(echo "Path:3")
(push)
(assert (and (= COL1 (_ bv111 32))))
(check-sat)
(get-model)
(pop)

(echo "Path:4")
(push)
(assert (and (= COL1 (_ bv333 32))))
(check-sat)
(get-model)
(pop)
