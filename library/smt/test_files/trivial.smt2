(set-logic QF_BV)
(declare-const x (_ BitVec 8))
(assert (bvugt x #x00))
(check-sat)
