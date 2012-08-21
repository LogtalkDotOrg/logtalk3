(define (problem os-netbenefit-p5_1)
(:domain openstacks-netbenefit-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5  - order
p1 p2 p3 p4 p5  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-in-use) 0)

(waiting o1)
(includes o1 p2)

(waiting o2)
(includes o2 p1)(includes o2 p2)

(waiting o3)
(includes o3 p3)

(waiting o4)
(includes o4 p3)(includes o4 p4)

(waiting o5)
(includes o5 p5)

(= (total-cost) 0)

(= (stack-cost) 2)

)

(:goal
(and
(shipped o1)
(shipped o2)
(shipped o3)
(shipped o4)
(shipped o5)
(preference d-o1-p2 (delivered o1 p2))
(preference d-o2-p1 (delivered o2 p1))
(preference d-o2-p2 (delivered o2 p2))
(preference d-o3-p3 (delivered o3 p3))
(preference d-o4-p3 (delivered o4 p3))
(preference d-o4-p4 (delivered o4 p4))
(preference d-o5-p5 (delivered o5 p5))
))

(:metric maximize (- 12 (+
(total-cost)
(* (is-violated d-o1-p2) 1)
(* (is-violated d-o2-p1) 1)
(* (is-violated d-o2-p2) 1)
(* (is-violated d-o3-p3) 1)
(* (is-violated d-o4-p3) 1)
(* (is-violated d-o4-p4) 1)
(* (is-violated d-o5-p5) 1)
)))
)
