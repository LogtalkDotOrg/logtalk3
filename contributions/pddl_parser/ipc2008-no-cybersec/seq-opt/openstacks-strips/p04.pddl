(define (problem os-sequencedstrips-p8_1)
(:domain openstacks-sequencedstrips-nonADL-nonNegated)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8  - count
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

(not-made p1)
(not-made p2)
(not-made p3)
(not-made p4)
(not-made p5)
(not-made p6)
(not-made p7)
(not-made p8)

(= (total-cost) 0)

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
))

(:metric minimize (total-cost))

)

