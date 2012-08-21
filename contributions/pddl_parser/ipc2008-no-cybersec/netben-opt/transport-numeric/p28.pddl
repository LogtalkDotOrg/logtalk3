; Transport two-cities-netbenefit-10nodes-700size-3degree-70mindistance-3trucks-5packages-6024seed

(define (problem transport-two-cities-netbenefit-10nodes-700size-3degree-70mindistance-3trucks-5packages-6024seed)
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
  truck-1 - vehicle
  truck-2 - vehicle
  truck-3 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
  package-4 - package
  package-5 - package
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
  ; 512,78 -> 606,306
  (road city-1-loc-2 city-1-loc-1)
  (= (road-length city-1-loc-2 city-1-loc-1) 25)
  (= (fuel-demand city-1-loc-2 city-1-loc-1) 50)
  ; 606,306 -> 512,78
  (road city-1-loc-1 city-1-loc-2)
  (= (road-length city-1-loc-1 city-1-loc-2) 25)
  (= (fuel-demand city-1-loc-1 city-1-loc-2) 50)
  ; 581,218 -> 606,306
  (road city-1-loc-3 city-1-loc-1)
  (= (road-length city-1-loc-3 city-1-loc-1) 10)
  (= (fuel-demand city-1-loc-3 city-1-loc-1) 19)
  ; 606,306 -> 581,218
  (road city-1-loc-1 city-1-loc-3)
  (= (road-length city-1-loc-1 city-1-loc-3) 10)
  (= (fuel-demand city-1-loc-1 city-1-loc-3) 19)
  ; 581,218 -> 512,78
  (road city-1-loc-3 city-1-loc-2)
  (= (road-length city-1-loc-3 city-1-loc-2) 16)
  (= (fuel-demand city-1-loc-3 city-1-loc-2) 32)
  ; 512,78 -> 581,218
  (road city-1-loc-2 city-1-loc-3)
  (= (road-length city-1-loc-2 city-1-loc-3) 16)
  (= (fuel-demand city-1-loc-2 city-1-loc-3) 32)
  ; 459,347 -> 606,306
  (road city-1-loc-5 city-1-loc-1)
  (= (road-length city-1-loc-5 city-1-loc-1) 16)
  (= (fuel-demand city-1-loc-5 city-1-loc-1) 31)
  ; 606,306 -> 459,347
  (road city-1-loc-1 city-1-loc-5)
  (= (road-length city-1-loc-1 city-1-loc-5) 16)
  (= (fuel-demand city-1-loc-1 city-1-loc-5) 31)
  ; 459,347 -> 581,218
  (road city-1-loc-5 city-1-loc-3)
  (= (road-length city-1-loc-5 city-1-loc-3) 18)
  (= (fuel-demand city-1-loc-5 city-1-loc-3) 36)
  ; 581,218 -> 459,347
  (road city-1-loc-3 city-1-loc-5)
  (= (road-length city-1-loc-3 city-1-loc-5) 18)
  (= (fuel-demand city-1-loc-3 city-1-loc-5) 36)
  ; 390,532 -> 459,347
  (road city-1-loc-6 city-1-loc-5)
  (= (road-length city-1-loc-6 city-1-loc-5) 20)
  (= (fuel-demand city-1-loc-6 city-1-loc-5) 40)
  ; 459,347 -> 390,532
  (road city-1-loc-5 city-1-loc-6)
  (= (road-length city-1-loc-5 city-1-loc-6) 20)
  (= (fuel-demand city-1-loc-5 city-1-loc-6) 40)
  ; 263,105 -> 512,78
  (road city-1-loc-7 city-1-loc-2)
  (= (road-length city-1-loc-7 city-1-loc-2) 25)
  (= (fuel-demand city-1-loc-7 city-1-loc-2) 50)
  ; 512,78 -> 263,105
  (road city-1-loc-2 city-1-loc-7)
  (= (road-length city-1-loc-2 city-1-loc-7) 25)
  (= (fuel-demand city-1-loc-2 city-1-loc-7) 50)
  ; 263,105 -> 35,217
  (road city-1-loc-7 city-1-loc-4)
  (= (road-length city-1-loc-7 city-1-loc-4) 26)
  (= (fuel-demand city-1-loc-7 city-1-loc-4) 51)
  ; 35,217 -> 263,105
  (road city-1-loc-4 city-1-loc-7)
  (= (road-length city-1-loc-4 city-1-loc-7) 26)
  (= (fuel-demand city-1-loc-4 city-1-loc-7) 51)
  ; 127,449 -> 35,217
  (road city-1-loc-8 city-1-loc-4)
  (= (road-length city-1-loc-8 city-1-loc-4) 25)
  (= (fuel-demand city-1-loc-8 city-1-loc-4) 50)
  ; 35,217 -> 127,449
  (road city-1-loc-4 city-1-loc-8)
  (= (road-length city-1-loc-4 city-1-loc-8) 25)
  (= (fuel-demand city-1-loc-4 city-1-loc-8) 50)
  ; 340,297 -> 581,218
  (road city-1-loc-9 city-1-loc-3)
  (= (road-length city-1-loc-9 city-1-loc-3) 26)
  (= (fuel-demand city-1-loc-9 city-1-loc-3) 51)
  ; 581,218 -> 340,297
  (road city-1-loc-3 city-1-loc-9)
  (= (road-length city-1-loc-3 city-1-loc-9) 26)
  (= (fuel-demand city-1-loc-3 city-1-loc-9) 51)
  ; 340,297 -> 459,347
  (road city-1-loc-9 city-1-loc-5)
  (= (road-length city-1-loc-9 city-1-loc-5) 13)
  (= (fuel-demand city-1-loc-9 city-1-loc-5) 26)
  ; 459,347 -> 340,297
  (road city-1-loc-5 city-1-loc-9)
  (= (road-length city-1-loc-5 city-1-loc-9) 13)
  (= (fuel-demand city-1-loc-5 city-1-loc-9) 26)
  ; 340,297 -> 390,532
  (road city-1-loc-9 city-1-loc-6)
  (= (road-length city-1-loc-9 city-1-loc-6) 24)
  (= (fuel-demand city-1-loc-9 city-1-loc-6) 48)
  ; 390,532 -> 340,297
  (road city-1-loc-6 city-1-loc-9)
  (= (road-length city-1-loc-6 city-1-loc-9) 24)
  (= (fuel-demand city-1-loc-6 city-1-loc-9) 48)
  ; 340,297 -> 263,105
  (road city-1-loc-9 city-1-loc-7)
  (= (road-length city-1-loc-9 city-1-loc-7) 21)
  (= (fuel-demand city-1-loc-9 city-1-loc-7) 42)
  ; 263,105 -> 340,297
  (road city-1-loc-7 city-1-loc-9)
  (= (road-length city-1-loc-7 city-1-loc-9) 21)
  (= (fuel-demand city-1-loc-7 city-1-loc-9) 42)
  ; 279,346 -> 459,347
  (road city-1-loc-10 city-1-loc-5)
  (= (road-length city-1-loc-10 city-1-loc-5) 18)
  (= (fuel-demand city-1-loc-10 city-1-loc-5) 36)
  ; 459,347 -> 279,346
  (road city-1-loc-5 city-1-loc-10)
  (= (road-length city-1-loc-5 city-1-loc-10) 18)
  (= (fuel-demand city-1-loc-5 city-1-loc-10) 36)
  ; 279,346 -> 390,532
  (road city-1-loc-10 city-1-loc-6)
  (= (road-length city-1-loc-10 city-1-loc-6) 22)
  (= (fuel-demand city-1-loc-10 city-1-loc-6) 44)
  ; 390,532 -> 279,346
  (road city-1-loc-6 city-1-loc-10)
  (= (road-length city-1-loc-6 city-1-loc-10) 22)
  (= (fuel-demand city-1-loc-6 city-1-loc-10) 44)
  ; 279,346 -> 263,105
  (road city-1-loc-10 city-1-loc-7)
  (= (road-length city-1-loc-10 city-1-loc-7) 25)
  (= (fuel-demand city-1-loc-10 city-1-loc-7) 49)
  ; 263,105 -> 279,346
  (road city-1-loc-7 city-1-loc-10)
  (= (road-length city-1-loc-7 city-1-loc-10) 25)
  (= (fuel-demand city-1-loc-7 city-1-loc-10) 49)
  ; 279,346 -> 127,449
  (road city-1-loc-10 city-1-loc-8)
  (= (road-length city-1-loc-10 city-1-loc-8) 19)
  (= (fuel-demand city-1-loc-10 city-1-loc-8) 37)
  ; 127,449 -> 279,346
  (road city-1-loc-8 city-1-loc-10)
  (= (road-length city-1-loc-8 city-1-loc-10) 19)
  (= (fuel-demand city-1-loc-8 city-1-loc-10) 37)
  ; 279,346 -> 340,297
  (road city-1-loc-10 city-1-loc-9)
  (= (road-length city-1-loc-10 city-1-loc-9) 8)
  (= (fuel-demand city-1-loc-10 city-1-loc-9) 16)
  ; 340,297 -> 279,346
  (road city-1-loc-9 city-1-loc-10)
  (= (road-length city-1-loc-9 city-1-loc-10) 8)
  (= (fuel-demand city-1-loc-9 city-1-loc-10) 16)
  ; 1790,452 -> 1697,501
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 11)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 21)
  ; 1697,501 -> 1790,452
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 11)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 21)
  ; 1790,452 -> 1857,279
  (road city-2-loc-3 city-2-loc-2)
  (= (road-length city-2-loc-3 city-2-loc-2) 19)
  (= (fuel-demand city-2-loc-3 city-2-loc-2) 38)
  ; 1857,279 -> 1790,452
  (road city-2-loc-2 city-2-loc-3)
  (= (road-length city-2-loc-2 city-2-loc-3) 19)
  (= (fuel-demand city-2-loc-2 city-2-loc-3) 38)
  ; 1516,330 -> 1697,501
  (road city-2-loc-4 city-2-loc-1)
  (= (road-length city-2-loc-4 city-2-loc-1) 25)
  (= (fuel-demand city-2-loc-4 city-2-loc-1) 50)
  ; 1697,501 -> 1516,330
  (road city-2-loc-1 city-2-loc-4)
  (= (road-length city-2-loc-1 city-2-loc-4) 25)
  (= (fuel-demand city-2-loc-1 city-2-loc-4) 50)
  ; 1718,360 -> 1697,501
  (road city-2-loc-5 city-2-loc-1)
  (= (road-length city-2-loc-5 city-2-loc-1) 15)
  (= (fuel-demand city-2-loc-5 city-2-loc-1) 29)
  ; 1697,501 -> 1718,360
  (road city-2-loc-1 city-2-loc-5)
  (= (road-length city-2-loc-1 city-2-loc-5) 15)
  (= (fuel-demand city-2-loc-1 city-2-loc-5) 29)
  ; 1718,360 -> 1857,279
  (road city-2-loc-5 city-2-loc-2)
  (= (road-length city-2-loc-5 city-2-loc-2) 17)
  (= (fuel-demand city-2-loc-5 city-2-loc-2) 33)
  ; 1857,279 -> 1718,360
  (road city-2-loc-2 city-2-loc-5)
  (= (road-length city-2-loc-2 city-2-loc-5) 17)
  (= (fuel-demand city-2-loc-2 city-2-loc-5) 33)
  ; 1718,360 -> 1790,452
  (road city-2-loc-5 city-2-loc-3)
  (= (road-length city-2-loc-5 city-2-loc-3) 12)
  (= (fuel-demand city-2-loc-5 city-2-loc-3) 24)
  ; 1790,452 -> 1718,360
  (road city-2-loc-3 city-2-loc-5)
  (= (road-length city-2-loc-3 city-2-loc-5) 12)
  (= (fuel-demand city-2-loc-3 city-2-loc-5) 24)
  ; 1718,360 -> 1516,330
  (road city-2-loc-5 city-2-loc-4)
  (= (road-length city-2-loc-5 city-2-loc-4) 21)
  (= (fuel-demand city-2-loc-5 city-2-loc-4) 41)
  ; 1516,330 -> 1718,360
  (road city-2-loc-4 city-2-loc-5)
  (= (road-length city-2-loc-4 city-2-loc-5) 21)
  (= (fuel-demand city-2-loc-4 city-2-loc-5) 41)
  ; 1572,692 -> 1697,501
  (road city-2-loc-6 city-2-loc-1)
  (= (road-length city-2-loc-6 city-2-loc-1) 23)
  (= (fuel-demand city-2-loc-6 city-2-loc-1) 46)
  ; 1697,501 -> 1572,692
  (road city-2-loc-1 city-2-loc-6)
  (= (road-length city-2-loc-1 city-2-loc-6) 23)
  (= (fuel-demand city-2-loc-1 city-2-loc-6) 46)
  ; 1432,519 -> 1516,330
  (road city-2-loc-7 city-2-loc-4)
  (= (road-length city-2-loc-7 city-2-loc-4) 21)
  (= (fuel-demand city-2-loc-7 city-2-loc-4) 42)
  ; 1516,330 -> 1432,519
  (road city-2-loc-4 city-2-loc-7)
  (= (road-length city-2-loc-4 city-2-loc-7) 21)
  (= (fuel-demand city-2-loc-4 city-2-loc-7) 42)
  ; 1432,519 -> 1572,692
  (road city-2-loc-7 city-2-loc-6)
  (= (road-length city-2-loc-7 city-2-loc-6) 23)
  (= (fuel-demand city-2-loc-7 city-2-loc-6) 45)
  ; 1572,692 -> 1432,519
  (road city-2-loc-6 city-2-loc-7)
  (= (road-length city-2-loc-6 city-2-loc-7) 23)
  (= (fuel-demand city-2-loc-6 city-2-loc-7) 45)
  ; 2030,536 -> 1790,452
  (road city-2-loc-8 city-2-loc-3)
  (= (road-length city-2-loc-8 city-2-loc-3) 26)
  (= (fuel-demand city-2-loc-8 city-2-loc-3) 51)
  ; 1790,452 -> 2030,536
  (road city-2-loc-3 city-2-loc-8)
  (= (road-length city-2-loc-3 city-2-loc-8) 26)
  (= (fuel-demand city-2-loc-3 city-2-loc-8) 51)
  ; 1462,86 -> 1516,330
  (road city-2-loc-9 city-2-loc-4)
  (= (road-length city-2-loc-9 city-2-loc-4) 25)
  (= (fuel-demand city-2-loc-9 city-2-loc-4) 50)
  ; 1516,330 -> 1462,86
  (road city-2-loc-4 city-2-loc-9)
  (= (road-length city-2-loc-4 city-2-loc-9) 25)
  (= (fuel-demand city-2-loc-4 city-2-loc-9) 50)
  ; 1950,547 -> 1697,501
  (road city-2-loc-10 city-2-loc-1)
  (= (road-length city-2-loc-10 city-2-loc-1) 26)
  (= (fuel-demand city-2-loc-10 city-2-loc-1) 52)
  ; 1697,501 -> 1950,547
  (road city-2-loc-1 city-2-loc-10)
  (= (road-length city-2-loc-1 city-2-loc-10) 26)
  (= (fuel-demand city-2-loc-1 city-2-loc-10) 52)
  ; 1950,547 -> 1790,452
  (road city-2-loc-10 city-2-loc-3)
  (= (road-length city-2-loc-10 city-2-loc-3) 19)
  (= (fuel-demand city-2-loc-10 city-2-loc-3) 38)
  ; 1790,452 -> 1950,547
  (road city-2-loc-3 city-2-loc-10)
  (= (road-length city-2-loc-3 city-2-loc-10) 19)
  (= (fuel-demand city-2-loc-3 city-2-loc-10) 38)
  ; 1950,547 -> 2030,536
  (road city-2-loc-10 city-2-loc-8)
  (= (road-length city-2-loc-10 city-2-loc-8) 9)
  (= (fuel-demand city-2-loc-10 city-2-loc-8) 17)
  ; 2030,536 -> 1950,547
  (road city-2-loc-8 city-2-loc-10)
  (= (road-length city-2-loc-8 city-2-loc-10) 9)
  (= (fuel-demand city-2-loc-8 city-2-loc-10) 17)
  ; 606,306 <-> 1432,519
  (road city-1-loc-1 city-2-loc-7)
  (= (road-length city-1-loc-1 city-2-loc-7) 86)
  (= (fuel-demand city-1-loc-1 city-2-loc-7) 43)
  (road city-2-loc-7 city-1-loc-1)
  (= (road-length city-2-loc-7 city-1-loc-1) 86)
  (= (fuel-demand city-2-loc-7 city-1-loc-1) 43)
  (has-petrol-station city-1-loc-1)
  (has-petrol-station city-2-loc-7)
  (at package-1 city-1-loc-5)
  (at package-2 city-1-loc-3)
  (at package-3 city-1-loc-5)
  (at package-4 city-1-loc-7)
  (at package-5 city-1-loc-7)
  (at truck-1 city-2-loc-7)
  (= (fuel-left truck-1) 639)
  (= (fuel-max truck-1) 639)
  (capacity truck-1 capacity-2)
  (at truck-2 city-2-loc-7)
  (= (fuel-left truck-2) 639)
  (= (fuel-max truck-2) 639)
  (capacity truck-2 capacity-3)
  (at truck-3 city-2-loc-9)
  (= (fuel-left truck-3) 639)
  (= (fuel-max truck-3) 639)
  (capacity truck-3 capacity-2)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-10))
  (preference delivery-2 (at package-2 city-2-loc-2))
  (preference delivery-3 (at package-3 city-2-loc-6))
  (preference delivery-4 (at package-4 city-2-loc-8))
  (preference delivery-5 (at package-5 city-2-loc-6))
 ))
 (:metric maximize
   (- 1300
     (+ (total-cost)
       (* (is-violated delivery-1) 251)
       (* (is-violated delivery-2) 214)
       (* (is-violated delivery-3) 245)
       (* (is-violated delivery-4) 292)
       (* (is-violated delivery-5) 298)
     )
   )
 )
)
