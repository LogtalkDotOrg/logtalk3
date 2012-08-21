(define (problem os-sequencedstrips-p100_3)
(:domain openstacks-sequencedstrips-ADL)
(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20 n21 n22 n23 n24 n25 n26 n27 n28 n29 n30 n31 n32 n33 n34 n35 n36 n37 n38 n39 n40 n41 n42 n43 n44 n45 n46 n47 n48 n49 n50 n51 n52 n53 n54 n55 n56 n57 n58 n59 n60 n61 n62 n63 n64 n65 n66 n67 n68 n69 n70 n71 n72 n73 n74 n75 n76 n77 n78 n79 n80 n81 n82 n83 n84 n85 n86 n87 n88 n89 n90 n91 n92 n93 n94 n95 n96 n97 n98 n99 n100  - count
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28 o29 o30 o31 o32 o33 o34 o35 o36 o37 o38 o39 o40 o41 o42 o43 o44 o45 o46 o47 o48 o49 o50 o51 o52 o53 o54 o55 o56 o57 o58 o59 o60 o61 o62 o63 o64 o65 o66 o67 o68 o69 o70 o71 o72 o73 o74 o75 o76 o77 o78 o79 o80 o81 o82 o83 o84 o85 o86 o87 o88 o89 o90 o91 o92 o93 o94 o95 o96 o97 o98 o99 o100  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47 p48 p49 p50 p51 p52 p53 p54 p55 p56 p57 p58 p59 p60 p61 p62 p63 p64 p65 p66 p67 p68 p69 p70 p71 p72 p73 p74 p75 p76 p77 p78 p79 p80 p81 p82 p83 p84 p85 p86 p87 p88 p89 p90 p91 p92 p93 p94 p95 p96 p97 p98 p99 p100  - product

)

(:init
(next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) (next-count n5 n6) (next-count n6 n7) (next-count n7 n8) (next-count n8 n9) (next-count n9 n10) (next-count n10 n11) (next-count n11 n12) (next-count n12 n13) (next-count n13 n14) (next-count n14 n15) (next-count n15 n16) (next-count n16 n17) (next-count n17 n18) (next-count n18 n19) (next-count n19 n20) (next-count n20 n21) (next-count n21 n22) (next-count n22 n23) (next-count n23 n24) (next-count n24 n25) (next-count n25 n26) (next-count n26 n27) (next-count n27 n28) (next-count n28 n29) (next-count n29 n30) (next-count n30 n31) (next-count n31 n32) (next-count n32 n33) (next-count n33 n34) (next-count n34 n35) (next-count n35 n36) (next-count n36 n37) (next-count n37 n38) (next-count n38 n39) (next-count n39 n40) (next-count n40 n41) (next-count n41 n42) (next-count n42 n43) (next-count n43 n44) (next-count n44 n45) (next-count n45 n46) (next-count n46 n47) (next-count n47 n48) (next-count n48 n49) (next-count n49 n50) (next-count n50 n51) (next-count n51 n52) (next-count n52 n53) (next-count n53 n54) (next-count n54 n55) (next-count n55 n56) (next-count n56 n57) (next-count n57 n58) (next-count n58 n59) (next-count n59 n60) (next-count n60 n61) (next-count n61 n62) (next-count n62 n63) (next-count n63 n64) (next-count n64 n65) (next-count n65 n66) (next-count n66 n67) (next-count n67 n68) (next-count n68 n69) (next-count n69 n70) (next-count n70 n71) (next-count n71 n72) (next-count n72 n73) (next-count n73 n74) (next-count n74 n75) (next-count n75 n76) (next-count n76 n77) (next-count n77 n78) (next-count n78 n79) (next-count n79 n80) (next-count n80 n81) (next-count n81 n82) (next-count n82 n83) (next-count n83 n84) (next-count n84 n85) (next-count n85 n86) (next-count n86 n87) (next-count n87 n88) (next-count n88 n89) (next-count n89 n90) (next-count n90 n91) (next-count n91 n92) (next-count n92 n93) (next-count n93 n94) (next-count n94 n95) (next-count n95 n96) (next-count n96 n97) (next-count n97 n98) (next-count n98 n99) (next-count n99 n100) 
(stacks-avail n0)

(waiting o1)
(includes o1 p84)

(waiting o2)
(includes o2 p1)(includes o2 p31)(includes o2 p44)(includes o2 p49)

(waiting o3)
(includes o3 p77)

(waiting o4)
(includes o4 p18)

(waiting o5)
(includes o5 p21)(includes o5 p31)

(waiting o6)
(includes o6 p4)(includes o6 p23)(includes o6 p24)(includes o6 p25)(includes o6 p29)(includes o6 p58)

(waiting o7)
(includes o7 p40)(includes o7 p44)(includes o7 p60)

(waiting o8)
(includes o8 p11)(includes o8 p16)(includes o8 p19)(includes o8 p72)

(waiting o9)
(includes o9 p2)(includes o9 p5)(includes o9 p11)(includes o9 p16)

(waiting o10)
(includes o10 p54)

(waiting o11)
(includes o11 p14)(includes o11 p15)(includes o11 p20)(includes o11 p28)(includes o11 p36)(includes o11 p53)(includes o11 p80)(includes o11 p84)

(waiting o12)
(includes o12 p75)

(waiting o13)
(includes o13 p34)(includes o13 p47)(includes o13 p51)(includes o13 p71)

(waiting o14)
(includes o14 p18)(includes o14 p20)(includes o14 p25)

(waiting o15)
(includes o15 p8)(includes o15 p16)(includes o15 p45)

(waiting o16)
(includes o16 p10)(includes o16 p14)(includes o16 p63)

(waiting o17)
(includes o17 p7)(includes o17 p12)

(waiting o18)
(includes o18 p10)(includes o18 p11)(includes o18 p19)(includes o18 p36)(includes o18 p66)

(waiting o19)
(includes o19 p43)

(waiting o20)
(includes o20 p64)(includes o20 p66)

(waiting o21)
(includes o21 p26)(includes o21 p76)(includes o21 p88)(includes o21 p89)

(waiting o22)
(includes o22 p57)(includes o22 p67)

(waiting o23)
(includes o23 p4)

(waiting o24)
(includes o24 p4)

(waiting o25)
(includes o25 p21)(includes o25 p22)(includes o25 p32)

(waiting o26)
(includes o26 p66)

(waiting o27)
(includes o27 p49)(includes o27 p59)

(waiting o28)
(includes o28 p23)(includes o28 p24)

(waiting o29)
(includes o29 p93)

(waiting o30)
(includes o30 p13)(includes o30 p38)(includes o30 p39)(includes o30 p47)(includes o30 p93)

(waiting o31)
(includes o31 p10)

(waiting o32)
(includes o32 p20)

(waiting o33)
(includes o33 p56)(includes o33 p60)

(waiting o34)
(includes o34 p83)(includes o34 p96)

(waiting o35)
(includes o35 p56)(includes o35 p79)

(waiting o36)
(includes o36 p15)

(waiting o37)
(includes o37 p13)(includes o37 p27)(includes o37 p28)(includes o37 p40)(includes o37 p66)

(waiting o38)
(includes o38 p14)(includes o38 p16)(includes o38 p56)(includes o38 p84)(includes o38 p89)(includes o38 p94)

(waiting o39)
(includes o39 p5)(includes o39 p15)(includes o39 p40)

(waiting o40)
(includes o40 p65)

(waiting o41)
(includes o41 p5)(includes o41 p25)(includes o41 p28)(includes o41 p37)(includes o41 p45)(includes o41 p51)

(waiting o42)
(includes o42 p70)(includes o42 p95)

(waiting o43)
(includes o43 p19)(includes o43 p25)

(waiting o44)
(includes o44 p33)(includes o44 p40)

(waiting o45)
(includes o45 p27)

(waiting o46)
(includes o46 p50)(includes o46 p52)(includes o46 p53)(includes o46 p68)(includes o46 p74)(includes o46 p93)

(waiting o47)
(includes o47 p66)

(waiting o48)
(includes o48 p29)(includes o48 p45)(includes o48 p62)(includes o48 p66)(includes o48 p82)

(waiting o49)
(includes o49 p4)(includes o49 p99)

(waiting o50)
(includes o50 p14)(includes o50 p29)(includes o50 p44)(includes o50 p45)(includes o50 p46)(includes o50 p59)(includes o50 p71)

(waiting o51)
(includes o51 p21)

(waiting o52)
(includes o52 p7)(includes o52 p28)(includes o52 p67)(includes o52 p81)

(waiting o53)
(includes o53 p62)(includes o53 p74)(includes o53 p78)

(waiting o54)
(includes o54 p53)(includes o54 p61)

(waiting o55)
(includes o55 p69)(includes o55 p82)(includes o55 p88)(includes o55 p93)(includes o55 p99)

(waiting o56)
(includes o56 p5)(includes o56 p13)(includes o56 p17)(includes o56 p30)

(waiting o57)
(includes o57 p16)

(waiting o58)
(includes o58 p62)(includes o58 p71)

(waiting o59)
(includes o59 p37)(includes o59 p51)

(waiting o60)
(includes o60 p38)(includes o60 p39)(includes o60 p66)(includes o60 p69)(includes o60 p78)

(waiting o61)
(includes o61 p9)(includes o61 p39)(includes o61 p52)(includes o61 p58)

(waiting o62)
(includes o62 p26)(includes o62 p41)(includes o62 p56)(includes o62 p58)(includes o62 p78)

(waiting o63)
(includes o63 p57)(includes o63 p67)(includes o63 p79)(includes o63 p85)

(waiting o64)
(includes o64 p52)

(waiting o65)
(includes o65 p22)(includes o65 p81)(includes o65 p82)(includes o65 p100)

(waiting o66)
(includes o66 p21)(includes o66 p56)(includes o66 p60)(includes o66 p65)(includes o66 p81)

(waiting o67)
(includes o67 p35)(includes o67 p43)(includes o67 p58)(includes o67 p73)

(waiting o68)
(includes o68 p10)(includes o68 p13)(includes o68 p33)(includes o68 p65)(includes o68 p89)

(waiting o69)
(includes o69 p5)(includes o69 p42)(includes o69 p71)

(waiting o70)
(includes o70 p28)(includes o70 p85)(includes o70 p100)

(waiting o71)
(includes o71 p24)(includes o71 p53)(includes o71 p79)(includes o71 p99)

(waiting o72)
(includes o72 p29)(includes o72 p50)(includes o72 p78)

(waiting o73)
(includes o73 p62)

(waiting o74)
(includes o74 p6)(includes o74 p28)(includes o74 p69)(includes o74 p79)

(waiting o75)
(includes o75 p82)(includes o75 p88)

(waiting o76)
(includes o76 p6)(includes o76 p35)(includes o76 p54)

(waiting o77)
(includes o77 p82)

(waiting o78)
(includes o78 p53)

(waiting o79)
(includes o79 p48)(includes o79 p49)(includes o79 p87)(includes o79 p94)

(waiting o80)
(includes o80 p75)(includes o80 p79)(includes o80 p86)(includes o80 p97)(includes o80 p100)

(waiting o81)
(includes o81 p11)(includes o81 p20)(includes o81 p44)(includes o81 p54)(includes o81 p55)(includes o81 p63)(includes o81 p82)

(waiting o82)
(includes o82 p92)

(waiting o83)
(includes o83 p84)(includes o83 p99)

(waiting o84)
(includes o84 p3)(includes o84 p12)(includes o84 p24)

(waiting o85)
(includes o85 p50)

(waiting o86)
(includes o86 p14)(includes o86 p34)(includes o86 p43)(includes o86 p62)

(waiting o87)
(includes o87 p56)

(waiting o88)
(includes o88 p15)(includes o88 p20)(includes o88 p59)(includes o88 p62)(includes o88 p91)

(waiting o89)
(includes o89 p33)(includes o89 p79)(includes o89 p86)

(waiting o90)
(includes o90 p13)(includes o90 p21)(includes o90 p32)(includes o90 p50)(includes o90 p56)(includes o90 p57)(includes o90 p62)(includes o90 p79)

(waiting o91)
(includes o91 p43)(includes o91 p56)

(waiting o92)
(includes o92 p53)(includes o92 p54)(includes o92 p66)(includes o92 p72)(includes o92 p75)

(waiting o93)
(includes o93 p10)

(waiting o94)
(includes o94 p18)(includes o94 p42)

(waiting o95)
(includes o95 p79)(includes o95 p98)

(waiting o96)
(includes o96 p13)(includes o96 p49)(includes o96 p64)

(waiting o97)
(includes o97 p70)(includes o97 p71)(includes o97 p74)(includes o97 p90)

(waiting o98)
(includes o98 p32)

(waiting o99)
(includes o99 p1)(includes o99 p5)(includes o99 p11)(includes o99 p12)

(waiting o100)
(includes o100 p53)

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
(shipped o51)
(shipped o52)
(shipped o53)
(shipped o54)
(shipped o55)
(shipped o56)
(shipped o57)
(shipped o58)
(shipped o59)
(shipped o60)
(shipped o61)
(shipped o62)
(shipped o63)
(shipped o64)
(shipped o65)
(shipped o66)
(shipped o67)
(shipped o68)
(shipped o69)
(shipped o70)
(shipped o71)
(shipped o72)
(shipped o73)
(shipped o74)
(shipped o75)
(shipped o76)
(shipped o77)
(shipped o78)
(shipped o79)
(shipped o80)
(shipped o81)
(shipped o82)
(shipped o83)
(shipped o84)
(shipped o85)
(shipped o86)
(shipped o87)
(shipped o88)
(shipped o89)
(shipped o90)
(shipped o91)
(shipped o92)
(shipped o93)
(shipped o94)
(shipped o95)
(shipped o96)
(shipped o97)
(shipped o98)
(shipped o99)
(shipped o100)
))

(:metric minimize (total-cost))

)

