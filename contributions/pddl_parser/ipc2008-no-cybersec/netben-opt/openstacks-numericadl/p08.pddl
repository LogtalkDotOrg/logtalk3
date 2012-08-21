(define (problem os-netbenefit-p12_1)
(:domain openstacks-netbenefit-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-in-use) 0)

(waiting o1)
(includes o1 p2)

(waiting o2)
(includes o2 p10)(includes o2 p11)

(waiting o3)
(includes o3 p4)

(waiting o4)
(includes o4 p4)

(waiting o5)
(includes o5 p6)(includes o5 p7)(includes o5 p9)

(waiting o6)
(includes o6 p3)(includes o6 p11)

(waiting o7)
(includes o7 p11)

(waiting o8)
(includes o8 p4)(includes o8 p6)

(waiting o9)
(includes o9 p11)

(waiting o10)
(includes o10 p1)(includes o10 p8)

(waiting o11)
(includes o11 p5)(includes o11 p11)(includes o11 p12)

(waiting o12)
(includes o12 p6)

(= (total-cost) 0)

(= (stack-cost) 6)

)

(:goal
(and
(shipped o1)
(shipped o2)
(shipped o3)
(shipped o4)
(shipped o5)
(shipped o6)
(shipped o7)
(shipped o8)
(shipped o9)
(shipped o10)
(shipped o11)
(shipped o12)
(preference d-o1-p2 (delivered o1 p2))
(preference d-o2-p10 (delivered o2 p10))
(preference d-o2-p11 (delivered o2 p11))
(preference d-o3-p4 (delivered o3 p4))
(preference d-o4-p4 (delivered o4 p4))
(preference d-o5-p6 (delivered o5 p6))
(preference d-o5-p7 (delivered o5 p7))
(preference d-o5-p9 (delivered o5 p9))
(preference d-o6-p3 (delivered o6 p3))
(preference d-o6-p11 (delivered o6 p11))
(preference d-o7-p11 (delivered o7 p11))
(preference d-o8-p4 (delivered o8 p4))
(preference d-o8-p6 (delivered o8 p6))
(preference d-o9-p11 (delivered o9 p11))
(preference d-o10-p1 (delivered o10 p1))
(preference d-o10-p8 (delivered o10 p8))
(preference d-o11-p5 (delivered o11 p5))
(preference d-o11-p11 (delivered o11 p11))
(preference d-o11-p12 (delivered o11 p12))
(preference d-o12-p6 (delivered o12 p6))
))

(:metric maximize (- 72 (+
(total-cost)
(* (is-violated d-o1-p2) 1)
(* (is-violated d-o2-p10) 1)
(* (is-violated d-o2-p11) 1)
(* (is-violated d-o3-p4) 1)
(* (is-violated d-o4-p4) 1)
(* (is-violated d-o5-p6) 1)
(* (is-violated d-o5-p7) 1)
(* (is-violated d-o5-p9) 1)
(* (is-violated d-o6-p3) 1)
(* (is-violated d-o6-p11) 1)
(* (is-violated d-o7-p11) 1)
(* (is-violated d-o8-p4) 1)
(* (is-violated d-o8-p6) 1)
(* (is-violated d-o9-p11) 1)
(* (is-violated d-o10-p1) 1)
(* (is-violated d-o10-p8) 1)
(* (is-violated d-o11-p5) 1)
(* (is-violated d-o11-p11) 1)
(* (is-violated d-o11-p12) 1)
(* (is-violated d-o12-p6) 1)
)))
)
