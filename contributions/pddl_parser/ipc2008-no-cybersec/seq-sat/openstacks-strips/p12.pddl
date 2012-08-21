(define (problem os-sequencedstrips-p20_3)
(:domain openstacks-sequencedstrips-nonADL-nonNegated)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20  - count
)

(:init
(next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) (next-count n5 n6) (next-count n6 n7) (next-count n7 n8) (next-count n8 n9) (next-count n9 n10) (next-count n10 n11) (next-count n11 n12) (next-count n12 n13) (next-count n13 n14) (next-count n14 n15) (next-count n15 n16) (next-count n16 n17) (next-count n17 n18) (next-count n18 n19) (next-count n19 n20) 
(stacks-avail n0)

(waiting o1)
(includes o1 p4)

(waiting o2)
(includes o2 p1)

(waiting o3)
(includes o3 p4)

(waiting o4)
(includes o4 p2)(includes o4 p3)(includes o4 p11)

(waiting o5)
(includes o5 p17)(includes o5 p20)

(waiting o6)
(includes o6 p9)

(waiting o7)
(includes o7 p6)(includes o7 p11)

(waiting o8)
(includes o8 p14)

(waiting o9)
(includes o9 p15)

(waiting o10)
(includes o10 p12)

(waiting o11)
(includes o11 p19)

(waiting o12)
(includes o12 p1)(includes o12 p3)

(waiting o13)
(includes o13 p11)(includes o13 p12)(includes o13 p18)

(waiting o14)
(includes o14 p7)(includes o14 p9)

(waiting o15)
(includes o15 p10)(includes o15 p12)(includes o15 p13)(includes o15 p14)

(waiting o16)
(includes o16 p5)

(waiting o17)
(includes o17 p16)

(waiting o18)
(includes o18 p8)(includes o18 p16)

(waiting o19)
(includes o19 p4)(includes o19 p10)(includes o19 p14)

(waiting o20)
(includes o20 p19)

(not-made p1)
(not-made p2)
(not-made p3)
(not-made p4)
(not-made p5)
(not-made p6)
(not-made p7)
(not-made p8)
(not-made p9)
(not-made p10)
(not-made p11)
(not-made p12)
(not-made p13)
(not-made p14)
(not-made p15)
(not-made p16)
(not-made p17)
(not-made p18)
(not-made p19)
(not-made p20)

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
(shipped o9)
(shipped o10)
(shipped o11)
(shipped o12)
(shipped o13)
(shipped o14)
(shipped o15)
(shipped o16)
(shipped o17)
(shipped o18)
(shipped o19)
(shipped o20)
))

(:metric minimize (total-cost))

)

