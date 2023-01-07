(set-logic SLIA)

(declare-const x (Seq Int))
(declare-const y (Seq Int))
(declare-const z (Seq Int))
(declare-const a Int)
(declare-const b Int)



(check-sat)
(get-model)
