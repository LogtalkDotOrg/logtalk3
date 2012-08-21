(define (problem os-netbenefit-p8_1)
(:domain openstacks-netbenefit-ADL)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8  - count
o1 o2 o3 o4 o5 o6 o7 o8  - order
p1 p2 p3 p4 p5 p6 p7 p8  - product

)

(:init
(next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) (next-count n5 n6) (next-count n6 n7) (next-count n7 n8) 
(stacks-avail n0)

(waiting o1)
(includes o1 p1)

(waiting o2)
(includes o2 p5)

(waiting o3)
(includes o3 p4)(includes o3 p8)

(waiting o4)
(includes o4 p7)

(waiting o5)
(includes o5 p2)(includes o5 p6)

(waiting o6)
(includes o6 p1)

(waiting o7)
(includes o7 p1)

(waiting o8)
(includes o8 p3)

(= (total-cost) 0)

(= (stack-cost) 4)

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
(preference d-o1-p1 (delivered o1 p1))
(preference d-o2-p5 (delivered o2 p5))
(preference d-o3-p4 (delivered o3 p4))
(preference d-o3-p8 (delivered o3 p8))
(preference d-o4-p7 (delivered o4 p7))
(preference d-o5-p2 (delivered o5 p2))
(preference d-o5-p6 (delivered o5 p6))
(preference d-o6-p1 (delivered o6 p1))
(preference d-o7-p1 (delivered o7 p1))
(preference d-o8-p3 (delivered o8 p3))
))

(:metric maximize (- 32 (+
(total-cost)
(* (is-violated d-o1-p1) 1)
(* (is-violated d-o2-p5) 1)
(* (is-violated d-o3-p4) 1)
(* (is-violated d-o3-p8) 1)
(* (is-violated d-o4-p7) 1)
(* (is-violated d-o5-p2) 1)
(* (is-violated d-o5-p6) 1)
(* (is-violated d-o6-p1) 1)
(* (is-violated d-o7-p1) 1)
(* (is-violated d-o8-p3) 1)
)))
)
