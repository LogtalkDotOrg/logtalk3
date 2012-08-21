(define (problem os-sequencedstrips-p30_2)
(:domain openstacks-sequencedstrips-ADL)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20 n21 n22 n23 n24 n25 n26 n27 n28 n29 n30  - count
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28 o29 o30  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30  - product

)

(:init
(next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) (next-count n5 n6) (next-count n6 n7) (next-count n7 n8) (next-count n8 n9) (next-count n9 n10) (next-count n10 n11) (next-count n11 n12) (next-count n12 n13) (next-count n13 n14) (next-count n14 n15) (next-count n15 n16) (next-count n16 n17) (next-count n17 n18) (next-count n18 n19) (next-count n19 n20) (next-count n20 n21) (next-count n21 n22) (next-count n22 n23) (next-count n23 n24) (next-count n24 n25) (next-count n25 n26) (next-count n26 n27) (next-count n27 n28) (next-count n28 n29) (next-count n29 n30) 
(stacks-avail n0)

(waiting o1)
(includes o1 p3)(includes o1 p11)

(waiting o2)
(includes o2 p10)

(waiting o3)
(includes o3 p16)(includes o3 p19)(includes o3 p21)

(waiting o4)
(includes o4 p15)

(waiting o5)
(includes o5 p18)(includes o5 p23)

(waiting o6)
(includes o6 p26)

(waiting o7)
(includes o7 p12)

(waiting o8)
(includes o8 p4)(includes o8 p29)

(waiting o9)
(includes o9 p2)(includes o9 p8)

(waiting o10)
(includes o10 p20)(includes o10 p27)

(waiting o11)
(includes o11 p9)(includes o11 p25)

(waiting o12)
(includes o12 p2)(includes o12 p3)(includes o12 p5)

(waiting o13)
(includes o13 p10)

(waiting o14)
(includes o14 p29)

(waiting o15)
(includes o15 p3)(includes o15 p5)(includes o15 p6)(includes o15 p8)

(waiting o16)
(includes o16 p12)

(waiting o17)
(includes o17 p25)

(waiting o18)
(includes o18 p11)(includes o18 p18)

(waiting o19)
(includes o19 p28)

(waiting o20)
(includes o20 p14)

(waiting o21)
(includes o21 p18)

(waiting o22)
(includes o22 p13)(includes o22 p14)(includes o22 p17)(includes o22 p22)(includes o22 p26)(includes o22 p30)

(waiting o23)
(includes o23 p21)

(waiting o24)
(includes o24 p7)(includes o24 p10)(includes o24 p15)(includes o24 p21)

(waiting o25)
(includes o25 p2)

(waiting o26)
(includes o26 p1)(includes o26 p16)

(waiting o27)
(includes o27 p17)(includes o27 p24)(includes o27 p28)

(waiting o28)
(includes o28 p2)(includes o28 p8)(includes o28 p28)

(waiting o29)
(includes o29 p12)

(waiting o30)
(includes o30 p26)

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
(shipped o26)
(shipped o27)
(shipped o28)
(shipped o29)
(shipped o30)
))

(:metric minimize (total-cost))

)

