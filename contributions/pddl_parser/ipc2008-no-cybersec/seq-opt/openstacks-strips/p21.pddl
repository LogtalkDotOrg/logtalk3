(define (problem os-sequencedstrips-p25_1)
(:domain openstacks-sequencedstrips-nonADL-nonNegated)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20 n21 n22 n23 n24 n25  - count
)

(:init
(next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) (next-count n5 n6) (next-count n6 n7) (next-count n7 n8) (next-count n8 n9) (next-count n9 n10) (next-count n10 n11) (next-count n11 n12) (next-count n12 n13) (next-count n13 n14) (next-count n14 n15) (next-count n15 n16) (next-count n16 n17) (next-count n17 n18) (next-count n18 n19) (next-count n19 n20) (next-count n20 n21) (next-count n21 n22) (next-count n22 n23) (next-count n23 n24) (next-count n24 n25) 
(stacks-avail n0)

(waiting o1)
(includes o1 p12)

(waiting o2)
(includes o2 p2)

(waiting o3)
(includes o3 p1)(includes o3 p4)(includes o3 p7)(includes o3 p13)

(waiting o4)
(includes o4 p2)

(waiting o5)
(includes o5 p6)(includes o5 p16)(includes o5 p24)

(waiting o6)
(includes o6 p22)

(waiting o7)
(includes o7 p20)

(waiting o8)
(includes o8 p8)

(waiting o9)
(includes o9 p3)

(waiting o10)
(includes o10 p1)

(waiting o11)
(includes o11 p12)

(waiting o12)
(includes o12 p8)(includes o12 p19)

(waiting o13)
(includes o13 p11)(includes o13 p14)(includes o13 p18)

(waiting o14)
(includes o14 p10)(includes o14 p18)

(waiting o15)
(includes o15 p4)(includes o15 p15)

(waiting o16)
(includes o16 p5)(includes o16 p20)

(waiting o17)
(includes o17 p9)

(waiting o18)
(includes o18 p14)

(waiting o19)
(includes o19 p17)(includes o19 p21)(includes o19 p25)

(waiting o20)
(includes o20 p24)

(waiting o21)
(includes o21 p9)

(waiting o22)
(includes o22 p22)(includes o22 p24)

(waiting o23)
(includes o23 p14)

(waiting o24)
(includes o24 p23)

(waiting o25)
(includes o25 p21)

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
(not-made p21)
(not-made p22)
(not-made p23)
(not-made p24)
(not-made p25)

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
(shipped o21)
(shipped o22)
(shipped o23)
(shipped o24)
(shipped o25)
))

(:metric minimize (total-cost))

)

