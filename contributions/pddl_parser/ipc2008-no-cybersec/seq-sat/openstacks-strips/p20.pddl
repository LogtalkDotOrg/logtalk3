(define (problem os-sequencedstrips-p50_2)
(:domain openstacks-sequencedstrips-nonADL-nonNegated)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20 n21 n22 n23 n24 n25 n26 n27 n28 n29 n30 n31 n32 n33 n34 n35 n36 n37 n38 n39 n40 n41 n42 n43 n44 n45 n46 n47 n48 n49 n50  - count
)

(:init
(next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) (next-count n5 n6) (next-count n6 n7) (next-count n7 n8) (next-count n8 n9) (next-count n9 n10) (next-count n10 n11) (next-count n11 n12) (next-count n12 n13) (next-count n13 n14) (next-count n14 n15) (next-count n15 n16) (next-count n16 n17) (next-count n17 n18) (next-count n18 n19) (next-count n19 n20) (next-count n20 n21) (next-count n21 n22) (next-count n22 n23) (next-count n23 n24) (next-count n24 n25) (next-count n25 n26) (next-count n26 n27) (next-count n27 n28) (next-count n28 n29) (next-count n29 n30) (next-count n30 n31) (next-count n31 n32) (next-count n32 n33) (next-count n33 n34) (next-count n34 n35) (next-count n35 n36) (next-count n36 n37) (next-count n37 n38) (next-count n38 n39) (next-count n39 n40) (next-count n40 n41) (next-count n41 n42) (next-count n42 n43) (next-count n43 n44) (next-count n44 n45) (next-count n45 n46) (next-count n46 n47) (next-count n47 n48) (next-count n48 n49) (next-count n49 n50) 
(stacks-avail n0)

(waiting o1)
(includes o1 p3)(includes o1 p15)

(waiting o2)
(includes o2 p6)

(waiting o3)
(includes o3 p9)(includes o3 p10)

(waiting o4)
(includes o4 p35)(includes o4 p44)

(waiting o5)
(includes o5 p34)(includes o5 p48)

(waiting o6)
(includes o6 p3)(includes o6 p8)(includes o6 p17)

(waiting o7)
(includes o7 p37)(includes o7 p42)

(waiting o8)
(includes o8 p8)

(waiting o9)
(includes o9 p6)(includes o9 p13)(includes o9 p23)

(waiting o10)
(includes o10 p20)(includes o10 p32)(includes o10 p35)

(waiting o11)
(includes o11 p43)

(waiting o12)
(includes o12 p20)(includes o12 p33)(includes o12 p38)(includes o12 p43)(includes o12 p45)

(waiting o13)
(includes o13 p14)(includes o13 p46)(includes o13 p48)

(waiting o14)
(includes o14 p23)

(waiting o15)
(includes o15 p30)(includes o15 p38)(includes o15 p49)(includes o15 p50)

(waiting o16)
(includes o16 p2)(includes o16 p23)(includes o16 p29)

(waiting o17)
(includes o17 p16)(includes o17 p24)

(waiting o18)
(includes o18 p19)(includes o18 p25)(includes o18 p26)(includes o18 p37)(includes o18 p47)

(waiting o19)
(includes o19 p22)(includes o19 p35)(includes o19 p42)

(waiting o20)
(includes o20 p4)(includes o20 p16)(includes o20 p18)(includes o20 p19)

(waiting o21)
(includes o21 p1)(includes o21 p7)(includes o21 p10)

(waiting o22)
(includes o22 p5)(includes o22 p12)(includes o22 p27)

(waiting o23)
(includes o23 p46)

(waiting o24)
(includes o24 p33)(includes o24 p41)(includes o24 p47)

(waiting o25)
(includes o25 p10)

(waiting o26)
(includes o26 p38)(includes o26 p40)(includes o26 p41)

(waiting o27)
(includes o27 p19)

(waiting o28)
(includes o28 p12)

(waiting o29)
(includes o29 p38)

(waiting o30)
(includes o30 p32)(includes o30 p46)

(waiting o31)
(includes o31 p11)(includes o31 p26)

(waiting o32)
(includes o32 p29)(includes o32 p36)

(waiting o33)
(includes o33 p7)(includes o33 p12)(includes o33 p16)(includes o33 p18)(includes o33 p19)

(waiting o34)
(includes o34 p28)(includes o34 p39)(includes o34 p42)(includes o34 p49)

(waiting o35)
(includes o35 p12)(includes o35 p25)(includes o35 p41)

(waiting o36)
(includes o36 p7)(includes o36 p20)

(waiting o37)
(includes o37 p8)

(waiting o38)
(includes o38 p19)(includes o38 p26)

(waiting o39)
(includes o39 p9)(includes o39 p16)(includes o39 p37)(includes o39 p38)

(waiting o40)
(includes o40 p6)(includes o40 p15)(includes o40 p17)(includes o40 p24)

(waiting o41)
(includes o41 p12)(includes o41 p13)

(waiting o42)
(includes o42 p26)

(waiting o43)
(includes o43 p33)(includes o43 p35)(includes o43 p46)

(waiting o44)
(includes o44 p17)(includes o44 p21)(includes o44 p28)(includes o44 p29)(includes o44 p31)(includes o44 p38)

(waiting o45)
(includes o45 p44)

(waiting o46)
(includes o46 p22)(includes o46 p25)

(waiting o47)
(includes o47 p42)

(waiting o48)
(includes o48 p42)

(waiting o49)
(includes o49 p12)(includes o49 p15)

(waiting o50)
(includes o50 p11)(includes o50 p39)

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
(not-made p26)
(not-made p27)
(not-made p28)
(not-made p29)
(not-made p30)
(not-made p31)
(not-made p32)
(not-made p33)
(not-made p34)
(not-made p35)
(not-made p36)
(not-made p37)
(not-made p38)
(not-made p39)
(not-made p40)
(not-made p41)
(not-made p42)
(not-made p43)
(not-made p44)
(not-made p45)
(not-made p46)
(not-made p47)
(not-made p48)
(not-made p49)
(not-made p50)

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
(shipped o31)
(shipped o32)
(shipped o33)
(shipped o34)
(shipped o35)
(shipped o36)
(shipped o37)
(shipped o38)
(shipped o39)
(shipped o40)
(shipped o41)
(shipped o42)
(shipped o43)
(shipped o44)
(shipped o45)
(shipped o46)
(shipped o47)
(shipped o48)
(shipped o49)
(shipped o50)
))

(:metric minimize (total-cost))

)

