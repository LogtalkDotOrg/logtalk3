; Transport two-cities-netbenefit-12nodes-700size-3degree-70mindistance-3trucks-6packages-6024seed

(define (problem transport-two-cities-netbenefit-12nodes-700size-3degree-70mindistance-3trucks-6packages-6024seed)
 (:domain transport)
 (:objects
  city-1-loc-1 - location
  city-2-loc-1 - location
  city-1-loc-2 - location
  city-2-loc-2 - location
  city-1-loc-3 - location
  city-2-loc-3 - location
  city-1-loc-4 - location
  city-2-loc-4 - location
  city-1-loc-5 - location
  city-2-loc-5 - location
  city-1-loc-6 - location
  city-2-loc-6 - location
  city-1-loc-7 - location
  city-2-loc-7 - location
  city-1-loc-8 - location
  city-2-loc-8 - location
  city-1-loc-9 - location
  city-2-loc-9 - location
  city-1-loc-10 - location
  city-2-loc-10 - location
  city-1-loc-11 - location
  city-2-loc-11 - location
  city-1-loc-12 - location
  city-2-loc-12 - location
  truck-1 - vehicle
  truck-2 - vehicle
  truck-3 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
  package-4 - package
  package-5 - package
  package-6 - package
  capacity-0 - capacity-number
  capacity-1 - capacity-number
  capacity-2 - capacity-number
  capacity-3 - capacity-number
  capacity-4 - capacity-number
 )
 (:init
  (= (total-cost) 0)
  (capacity-predecessor capacity-0 capacity-1)
  (capacity-predecessor capacity-1 capacity-2)
  (capacity-predecessor capacity-2 capacity-3)
  (capacity-predecessor capacity-3 capacity-4)
  ; 625,419 -> 507,269
  (road city-1-loc-2 city-1-loc-1)
  (= (road-length city-1-loc-2 city-1-loc-1) 20)
  (= (fuel-demand city-1-loc-2 city-1-loc-1) 39)
  ; 507,269 -> 625,419
  (road city-1-loc-1 city-1-loc-2)
  (= (road-length city-1-loc-1 city-1-loc-2) 20)
  (= (fuel-demand city-1-loc-1 city-1-loc-2) 39)
  ; 578,169 -> 507,269
  (road city-1-loc-3 city-1-loc-1)
  (= (road-length city-1-loc-3 city-1-loc-1) 13)
  (= (fuel-demand city-1-loc-3 city-1-loc-1) 25)
  ; 507,269 -> 578,169
  (road city-1-loc-1 city-1-loc-3)
  (= (road-length city-1-loc-1 city-1-loc-3) 13)
  (= (fuel-demand city-1-loc-1 city-1-loc-3) 25)
  ; 393,93 -> 507,269
  (road city-1-loc-4 city-1-loc-1)
  (= (road-length city-1-loc-4 city-1-loc-1) 21)
  (= (fuel-demand city-1-loc-4 city-1-loc-1) 42)
  ; 507,269 -> 393,93
  (road city-1-loc-1 city-1-loc-4)
  (= (road-length city-1-loc-1 city-1-loc-4) 21)
  (= (fuel-demand city-1-loc-1 city-1-loc-4) 42)
  ; 393,93 -> 578,169
  (road city-1-loc-4 city-1-loc-3)
  (= (road-length city-1-loc-4 city-1-loc-3) 20)
  (= (fuel-demand city-1-loc-4 city-1-loc-3) 40)
  ; 578,169 -> 393,93
  (road city-1-loc-3 city-1-loc-4)
  (= (road-length city-1-loc-3 city-1-loc-4) 20)
  (= (fuel-demand city-1-loc-3 city-1-loc-4) 40)
  ; 196,159 -> 393,93
  (road city-1-loc-6 city-1-loc-4)
  (= (road-length city-1-loc-6 city-1-loc-4) 21)
  (= (fuel-demand city-1-loc-6 city-1-loc-4) 42)
  ; 393,93 -> 196,159
  (road city-1-loc-4 city-1-loc-6)
  (= (road-length city-1-loc-4 city-1-loc-6) 21)
  (= (fuel-demand city-1-loc-4 city-1-loc-6) 42)
  ; 196,159 -> 102,242
  (road city-1-loc-6 city-1-loc-5)
  (= (road-length city-1-loc-6 city-1-loc-5) 13)
  (= (fuel-demand city-1-loc-6 city-1-loc-5) 25)
  ; 102,242 -> 196,159
  (road city-1-loc-5 city-1-loc-6)
  (= (road-length city-1-loc-5 city-1-loc-6) 13)
  (= (fuel-demand city-1-loc-5 city-1-loc-6) 25)
  ; 470,575 -> 625,419
  (road city-1-loc-7 city-1-loc-2)
  (= (road-length city-1-loc-7 city-1-loc-2) 22)
  (= (fuel-demand city-1-loc-7 city-1-loc-2) 44)
  ; 625,419 -> 470,575
  (road city-1-loc-2 city-1-loc-7)
  (= (road-length city-1-loc-2 city-1-loc-7) 22)
  (= (fuel-demand city-1-loc-2 city-1-loc-7) 44)
  ; 381,378 -> 507,269
  (road city-1-loc-8 city-1-loc-1)
  (= (road-length city-1-loc-8 city-1-loc-1) 17)
  (= (fuel-demand city-1-loc-8 city-1-loc-1) 34)
  ; 507,269 -> 381,378
  (road city-1-loc-1 city-1-loc-8)
  (= (road-length city-1-loc-1 city-1-loc-8) 17)
  (= (fuel-demand city-1-loc-1 city-1-loc-8) 34)
  ; 381,378 -> 470,575
  (road city-1-loc-8 city-1-loc-7)
  (= (road-length city-1-loc-8 city-1-loc-7) 22)
  (= (fuel-demand city-1-loc-8 city-1-loc-7) 44)
  ; 470,575 -> 381,378
  (road city-1-loc-7 city-1-loc-8)
  (= (road-length city-1-loc-7 city-1-loc-8) 22)
  (= (fuel-demand city-1-loc-7 city-1-loc-8) 44)
  ; 614,290 -> 507,269
  (road city-1-loc-9 city-1-loc-1)
  (= (road-length city-1-loc-9 city-1-loc-1) 11)
  (= (fuel-demand city-1-loc-9 city-1-loc-1) 22)
  ; 507,269 -> 614,290
  (road city-1-loc-1 city-1-loc-9)
  (= (road-length city-1-loc-1 city-1-loc-9) 11)
  (= (fuel-demand city-1-loc-1 city-1-loc-9) 22)
  ; 614,290 -> 625,419
  (road city-1-loc-9 city-1-loc-2)
  (= (road-length city-1-loc-9 city-1-loc-2) 13)
  (= (fuel-demand city-1-loc-9 city-1-loc-2) 26)
  ; 625,419 -> 614,290
  (road city-1-loc-2 city-1-loc-9)
  (= (road-length city-1-loc-2 city-1-loc-9) 13)
  (= (fuel-demand city-1-loc-2 city-1-loc-9) 26)
  ; 614,290 -> 578,169
  (road city-1-loc-9 city-1-loc-3)
  (= (road-length city-1-loc-9 city-1-loc-3) 13)
  (= (fuel-demand city-1-loc-9 city-1-loc-3) 26)
  ; 578,169 -> 614,290
  (road city-1-loc-3 city-1-loc-9)
  (= (road-length city-1-loc-3 city-1-loc-9) 13)
  (= (fuel-demand city-1-loc-3 city-1-loc-9) 26)
  ; 249,90 -> 393,93
  (road city-1-loc-10 city-1-loc-4)
  (= (road-length city-1-loc-10 city-1-loc-4) 15)
  (= (fuel-demand city-1-loc-10 city-1-loc-4) 29)
  ; 393,93 -> 249,90
  (road city-1-loc-4 city-1-loc-10)
  (= (road-length city-1-loc-4 city-1-loc-10) 15)
  (= (fuel-demand city-1-loc-4 city-1-loc-10) 29)
  ; 249,90 -> 102,242
  (road city-1-loc-10 city-1-loc-5)
  (= (road-length city-1-loc-10 city-1-loc-5) 22)
  (= (fuel-demand city-1-loc-10 city-1-loc-5) 43)
  ; 102,242 -> 249,90
  (road city-1-loc-5 city-1-loc-10)
  (= (road-length city-1-loc-5 city-1-loc-10) 22)
  (= (fuel-demand city-1-loc-5 city-1-loc-10) 43)
  ; 249,90 -> 196,159
  (road city-1-loc-10 city-1-loc-6)
  (= (road-length city-1-loc-10 city-1-loc-6) 9)
  (= (fuel-demand city-1-loc-10 city-1-loc-6) 18)
  ; 196,159 -> 249,90
  (road city-1-loc-6 city-1-loc-10)
  (= (road-length city-1-loc-6 city-1-loc-10) 9)
  (= (fuel-demand city-1-loc-6 city-1-loc-10) 18)
  ; 652,193 -> 507,269
  (road city-1-loc-11 city-1-loc-1)
  (= (road-length city-1-loc-11 city-1-loc-1) 17)
  (= (fuel-demand city-1-loc-11 city-1-loc-1) 33)
  ; 507,269 -> 652,193
  (road city-1-loc-1 city-1-loc-11)
  (= (road-length city-1-loc-1 city-1-loc-11) 17)
  (= (fuel-demand city-1-loc-1 city-1-loc-11) 33)
  ; 652,193 -> 625,419
  (road city-1-loc-11 city-1-loc-2)
  (= (road-length city-1-loc-11 city-1-loc-2) 23)
  (= (fuel-demand city-1-loc-11 city-1-loc-2) 46)
  ; 625,419 -> 652,193
  (road city-1-loc-2 city-1-loc-11)
  (= (road-length city-1-loc-2 city-1-loc-11) 23)
  (= (fuel-demand city-1-loc-2 city-1-loc-11) 46)
  ; 652,193 -> 578,169
  (road city-1-loc-11 city-1-loc-3)
  (= (road-length city-1-loc-11 city-1-loc-3) 8)
  (= (fuel-demand city-1-loc-11 city-1-loc-3) 16)
  ; 578,169 -> 652,193
  (road city-1-loc-3 city-1-loc-11)
  (= (road-length city-1-loc-3 city-1-loc-11) 8)
  (= (fuel-demand city-1-loc-3 city-1-loc-11) 16)
  ; 652,193 -> 614,290
  (road city-1-loc-11 city-1-loc-9)
  (= (road-length city-1-loc-11 city-1-loc-9) 11)
  (= (fuel-demand city-1-loc-11 city-1-loc-9) 21)
  ; 614,290 -> 652,193
  (road city-1-loc-9 city-1-loc-11)
  (= (road-length city-1-loc-9 city-1-loc-11) 11)
  (= (fuel-demand city-1-loc-9 city-1-loc-11) 21)
  ; 157,69 -> 393,93
  (road city-1-loc-12 city-1-loc-4)
  (= (road-length city-1-loc-12 city-1-loc-4) 24)
  (= (fuel-demand city-1-loc-12 city-1-loc-4) 48)
  ; 393,93 -> 157,69
  (road city-1-loc-4 city-1-loc-12)
  (= (road-length city-1-loc-4 city-1-loc-12) 24)
  (= (fuel-demand city-1-loc-4 city-1-loc-12) 48)
  ; 157,69 -> 102,242
  (road city-1-loc-12 city-1-loc-5)
  (= (road-length city-1-loc-12 city-1-loc-5) 19)
  (= (fuel-demand city-1-loc-12 city-1-loc-5) 37)
  ; 102,242 -> 157,69
  (road city-1-loc-5 city-1-loc-12)
  (= (road-length city-1-loc-5 city-1-loc-12) 19)
  (= (fuel-demand city-1-loc-5 city-1-loc-12) 37)
  ; 157,69 -> 196,159
  (road city-1-loc-12 city-1-loc-6)
  (= (road-length city-1-loc-12 city-1-loc-6) 10)
  (= (fuel-demand city-1-loc-12 city-1-loc-6) 20)
  ; 196,159 -> 157,69
  (road city-1-loc-6 city-1-loc-12)
  (= (road-length city-1-loc-6 city-1-loc-12) 10)
  (= (fuel-demand city-1-loc-6 city-1-loc-12) 20)
  ; 157,69 -> 249,90
  (road city-1-loc-12 city-1-loc-10)
  (= (road-length city-1-loc-12 city-1-loc-10) 10)
  (= (fuel-demand city-1-loc-12 city-1-loc-10) 19)
  ; 249,90 -> 157,69
  (road city-1-loc-10 city-1-loc-12)
  (= (road-length city-1-loc-10 city-1-loc-12) 10)
  (= (fuel-demand city-1-loc-10 city-1-loc-12) 19)
  ; 1632,662 -> 1587,459
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 21)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 42)
  ; 1587,459 -> 1632,662
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 21)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 42)
  ; 1525,662 -> 1587,459
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 22)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 43)
  ; 1587,459 -> 1525,662
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 22)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 43)
  ; 1525,662 -> 1632,662
  (road city-2-loc-3 city-2-loc-2)
  (= (road-length city-2-loc-3 city-2-loc-2) 11)
  (= (fuel-demand city-2-loc-3 city-2-loc-2) 22)
  ; 1632,662 -> 1525,662
  (road city-2-loc-2 city-2-loc-3)
  (= (road-length city-2-loc-2 city-2-loc-3) 11)
  (= (fuel-demand city-2-loc-2 city-2-loc-3) 22)
  ; 1574,319 -> 1587,459
  (road city-2-loc-4 city-2-loc-1)
  (= (road-length city-2-loc-4 city-2-loc-1) 15)
  (= (fuel-demand city-2-loc-4 city-2-loc-1) 29)
  ; 1587,459 -> 1574,319
  (road city-2-loc-1 city-2-loc-4)
  (= (road-length city-2-loc-1 city-2-loc-4) 15)
  (= (fuel-demand city-2-loc-1 city-2-loc-4) 29)
  ; 1526,416 -> 1587,459
  (road city-2-loc-5 city-2-loc-1)
  (= (road-length city-2-loc-5 city-2-loc-1) 8)
  (= (fuel-demand city-2-loc-5 city-2-loc-1) 15)
  ; 1587,459 -> 1526,416
  (road city-2-loc-1 city-2-loc-5)
  (= (road-length city-2-loc-1 city-2-loc-5) 8)
  (= (fuel-demand city-2-loc-1 city-2-loc-5) 15)
  ; 1526,416 -> 1574,319
  (road city-2-loc-5 city-2-loc-4)
  (= (road-length city-2-loc-5 city-2-loc-4) 11)
  (= (fuel-demand city-2-loc-5 city-2-loc-4) 22)
  ; 1574,319 -> 1526,416
  (road city-2-loc-4 city-2-loc-5)
  (= (road-length city-2-loc-4 city-2-loc-5) 11)
  (= (fuel-demand city-2-loc-4 city-2-loc-5) 22)
  ; 1907,342 -> 1851,230
  (road city-2-loc-8 city-2-loc-6)
  (= (road-length city-2-loc-8 city-2-loc-6) 13)
  (= (fuel-demand city-2-loc-8 city-2-loc-6) 25)
  ; 1851,230 -> 1907,342
  (road city-2-loc-6 city-2-loc-8)
  (= (road-length city-2-loc-6 city-2-loc-8) 13)
  (= (fuel-demand city-2-loc-6 city-2-loc-8) 25)
  ; 1779,140 -> 1851,230
  (road city-2-loc-9 city-2-loc-6)
  (= (road-length city-2-loc-9 city-2-loc-6) 12)
  (= (fuel-demand city-2-loc-9 city-2-loc-6) 23)
  ; 1851,230 -> 1779,140
  (road city-2-loc-6 city-2-loc-9)
  (= (road-length city-2-loc-6 city-2-loc-9) 12)
  (= (fuel-demand city-2-loc-6 city-2-loc-9) 23)
  ; 1779,140 -> 1989,33
  (road city-2-loc-9 city-2-loc-7)
  (= (road-length city-2-loc-9 city-2-loc-7) 24)
  (= (fuel-demand city-2-loc-9 city-2-loc-7) 48)
  ; 1989,33 -> 1779,140
  (road city-2-loc-7 city-2-loc-9)
  (= (road-length city-2-loc-7 city-2-loc-9) 24)
  (= (fuel-demand city-2-loc-7 city-2-loc-9) 48)
  ; 1753,510 -> 1587,459
  (road city-2-loc-10 city-2-loc-1)
  (= (road-length city-2-loc-10 city-2-loc-1) 18)
  (= (fuel-demand city-2-loc-10 city-2-loc-1) 35)
  ; 1587,459 -> 1753,510
  (road city-2-loc-1 city-2-loc-10)
  (= (road-length city-2-loc-1 city-2-loc-10) 18)
  (= (fuel-demand city-2-loc-1 city-2-loc-10) 35)
  ; 1753,510 -> 1632,662
  (road city-2-loc-10 city-2-loc-2)
  (= (road-length city-2-loc-10 city-2-loc-2) 20)
  (= (fuel-demand city-2-loc-10 city-2-loc-2) 39)
  ; 1632,662 -> 1753,510
  (road city-2-loc-2 city-2-loc-10)
  (= (road-length city-2-loc-2 city-2-loc-10) 20)
  (= (fuel-demand city-2-loc-2 city-2-loc-10) 39)
  ; 1753,510 -> 1907,342
  (road city-2-loc-10 city-2-loc-8)
  (= (road-length city-2-loc-10 city-2-loc-8) 23)
  (= (fuel-demand city-2-loc-10 city-2-loc-8) 46)
  ; 1907,342 -> 1753,510
  (road city-2-loc-8 city-2-loc-10)
  (= (road-length city-2-loc-8 city-2-loc-10) 23)
  (= (fuel-demand city-2-loc-8 city-2-loc-10) 46)
  ; 1921,448 -> 1851,230
  (road city-2-loc-11 city-2-loc-6)
  (= (road-length city-2-loc-11 city-2-loc-6) 23)
  (= (fuel-demand city-2-loc-11 city-2-loc-6) 46)
  ; 1851,230 -> 1921,448
  (road city-2-loc-6 city-2-loc-11)
  (= (road-length city-2-loc-6 city-2-loc-11) 23)
  (= (fuel-demand city-2-loc-6 city-2-loc-11) 46)
  ; 1921,448 -> 1907,342
  (road city-2-loc-11 city-2-loc-8)
  (= (road-length city-2-loc-11 city-2-loc-8) 11)
  (= (fuel-demand city-2-loc-11 city-2-loc-8) 22)
  ; 1907,342 -> 1921,448
  (road city-2-loc-8 city-2-loc-11)
  (= (road-length city-2-loc-8 city-2-loc-11) 11)
  (= (fuel-demand city-2-loc-8 city-2-loc-11) 22)
  ; 1921,448 -> 1753,510
  (road city-2-loc-11 city-2-loc-10)
  (= (road-length city-2-loc-11 city-2-loc-10) 18)
  (= (fuel-demand city-2-loc-11 city-2-loc-10) 36)
  ; 1753,510 -> 1921,448
  (road city-2-loc-10 city-2-loc-11)
  (= (road-length city-2-loc-10 city-2-loc-11) 18)
  (= (fuel-demand city-2-loc-10 city-2-loc-11) 36)
  ; 1498,565 -> 1587,459
  (road city-2-loc-12 city-2-loc-1)
  (= (road-length city-2-loc-12 city-2-loc-1) 14)
  (= (fuel-demand city-2-loc-12 city-2-loc-1) 28)
  ; 1587,459 -> 1498,565
  (road city-2-loc-1 city-2-loc-12)
  (= (road-length city-2-loc-1 city-2-loc-12) 14)
  (= (fuel-demand city-2-loc-1 city-2-loc-12) 28)
  ; 1498,565 -> 1632,662
  (road city-2-loc-12 city-2-loc-2)
  (= (road-length city-2-loc-12 city-2-loc-2) 17)
  (= (fuel-demand city-2-loc-12 city-2-loc-2) 33)
  ; 1632,662 -> 1498,565
  (road city-2-loc-2 city-2-loc-12)
  (= (road-length city-2-loc-2 city-2-loc-12) 17)
  (= (fuel-demand city-2-loc-2 city-2-loc-12) 33)
  ; 1498,565 -> 1525,662
  (road city-2-loc-12 city-2-loc-3)
  (= (road-length city-2-loc-12 city-2-loc-3) 11)
  (= (fuel-demand city-2-loc-12 city-2-loc-3) 21)
  ; 1525,662 -> 1498,565
  (road city-2-loc-3 city-2-loc-12)
  (= (road-length city-2-loc-3 city-2-loc-12) 11)
  (= (fuel-demand city-2-loc-3 city-2-loc-12) 21)
  ; 1498,565 -> 1526,416
  (road city-2-loc-12 city-2-loc-5)
  (= (road-length city-2-loc-12 city-2-loc-5) 16)
  (= (fuel-demand city-2-loc-12 city-2-loc-5) 31)
  ; 1526,416 -> 1498,565
  (road city-2-loc-5 city-2-loc-12)
  (= (road-length city-2-loc-5 city-2-loc-12) 16)
  (= (fuel-demand city-2-loc-5 city-2-loc-12) 31)
  ; 625,419 <-> 1498,565
  (road city-1-loc-2 city-2-loc-12)
  (= (road-length city-1-loc-2 city-2-loc-12) 89)
  (= (fuel-demand city-1-loc-2 city-2-loc-12) 45)
  (road city-2-loc-12 city-1-loc-2)
  (= (road-length city-2-loc-12 city-1-loc-2) 89)
  (= (fuel-demand city-2-loc-12 city-1-loc-2) 45)
  (has-petrol-station city-1-loc-2)
  (has-petrol-station city-2-loc-12)
  (at package-1 city-1-loc-8)
  (at package-2 city-1-loc-10)
  (at package-3 city-1-loc-1)
  (at package-4 city-1-loc-5)
  (at package-5 city-1-loc-11)
  (at package-6 city-1-loc-12)
  (at truck-1 city-2-loc-7)
  (= (fuel-left truck-1) 663)
  (= (fuel-max truck-1) 663)
  (capacity truck-1 capacity-4)
  (at truck-2 city-2-loc-10)
  (= (fuel-left truck-2) 663)
  (= (fuel-max truck-2) 663)
  (capacity truck-2 capacity-4)
  (at truck-3 city-2-loc-6)
  (= (fuel-left truck-3) 663)
  (= (fuel-max truck-3) 663)
  (capacity truck-3 capacity-3)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-12))
  (preference delivery-2 (at package-2 city-2-loc-3))
  (preference delivery-3 (at package-3 city-2-loc-9))
  (preference delivery-4 (at package-4 city-2-loc-7))
  (preference delivery-5 (at package-5 city-2-loc-10))
  (preference delivery-6 (at package-6 city-2-loc-7))
 ))
 (:metric maximize
   (- 1519
     (+ (total-cost)
       (* (is-violated delivery-1) 299)
       (* (is-violated delivery-2) 275)
       (* (is-violated delivery-3) 235)
       (* (is-violated delivery-4) 242)
       (* (is-violated delivery-5) 205)
       (* (is-violated delivery-6) 263)
     )
   )
 )
)
