; Transport two-cities-netbenefit-9nodes-700size-3degree-70mindistance-3trucks-5packages-6024seed

(define (problem transport-two-cities-netbenefit-9nodes-700size-3degree-70mindistance-3trucks-5packages-6024seed)
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
  (road city-1-loc-3 city-1-loc-2)
  (= (road-length city-1-loc-3 city-1-loc-2) 25)
  (= (fuel-demand city-1-loc-3 city-1-loc-2) 50)
  ; 606,306 -> 512,78
  (road city-1-loc-2 city-1-loc-3)
  (= (road-length city-1-loc-2 city-1-loc-3) 25)
  (= (fuel-demand city-1-loc-2 city-1-loc-3) 50)
  ; 581,218 -> 606,306
  (road city-1-loc-4 city-1-loc-2)
  (= (road-length city-1-loc-4 city-1-loc-2) 10)
  (= (fuel-demand city-1-loc-4 city-1-loc-2) 19)
  ; 606,306 -> 581,218
  (road city-1-loc-2 city-1-loc-4)
  (= (road-length city-1-loc-2 city-1-loc-4) 10)
  (= (fuel-demand city-1-loc-2 city-1-loc-4) 19)
  ; 581,218 -> 512,78
  (road city-1-loc-4 city-1-loc-3)
  (= (road-length city-1-loc-4 city-1-loc-3) 16)
  (= (fuel-demand city-1-loc-4 city-1-loc-3) 32)
  ; 512,78 -> 581,218
  (road city-1-loc-3 city-1-loc-4)
  (= (road-length city-1-loc-3 city-1-loc-4) 16)
  (= (fuel-demand city-1-loc-3 city-1-loc-4) 32)
  ; 35,217 -> 208,274
  (road city-1-loc-5 city-1-loc-1)
  (= (road-length city-1-loc-5 city-1-loc-1) 19)
  (= (fuel-demand city-1-loc-5 city-1-loc-1) 37)
  ; 208,274 -> 35,217
  (road city-1-loc-1 city-1-loc-5)
  (= (road-length city-1-loc-1 city-1-loc-5) 19)
  (= (fuel-demand city-1-loc-1 city-1-loc-5) 37)
  ; 459,347 -> 208,274
  (road city-1-loc-6 city-1-loc-1)
  (= (road-length city-1-loc-6 city-1-loc-1) 27)
  (= (fuel-demand city-1-loc-6 city-1-loc-1) 53)
  ; 208,274 -> 459,347
  (road city-1-loc-1 city-1-loc-6)
  (= (road-length city-1-loc-1 city-1-loc-6) 27)
  (= (fuel-demand city-1-loc-1 city-1-loc-6) 53)
  ; 459,347 -> 606,306
  (road city-1-loc-6 city-1-loc-2)
  (= (road-length city-1-loc-6 city-1-loc-2) 16)
  (= (fuel-demand city-1-loc-6 city-1-loc-2) 31)
  ; 606,306 -> 459,347
  (road city-1-loc-2 city-1-loc-6)
  (= (road-length city-1-loc-2 city-1-loc-6) 16)
  (= (fuel-demand city-1-loc-2 city-1-loc-6) 31)
  ; 459,347 -> 581,218
  (road city-1-loc-6 city-1-loc-4)
  (= (road-length city-1-loc-6 city-1-loc-4) 18)
  (= (fuel-demand city-1-loc-6 city-1-loc-4) 36)
  ; 581,218 -> 459,347
  (road city-1-loc-4 city-1-loc-6)
  (= (road-length city-1-loc-4 city-1-loc-6) 18)
  (= (fuel-demand city-1-loc-4 city-1-loc-6) 36)
  ; 390,532 -> 459,347
  (road city-1-loc-7 city-1-loc-6)
  (= (road-length city-1-loc-7 city-1-loc-6) 20)
  (= (fuel-demand city-1-loc-7 city-1-loc-6) 40)
  ; 459,347 -> 390,532
  (road city-1-loc-6 city-1-loc-7)
  (= (road-length city-1-loc-6 city-1-loc-7) 20)
  (= (fuel-demand city-1-loc-6 city-1-loc-7) 40)
  ; 263,105 -> 208,274
  (road city-1-loc-8 city-1-loc-1)
  (= (road-length city-1-loc-8 city-1-loc-1) 18)
  (= (fuel-demand city-1-loc-8 city-1-loc-1) 36)
  ; 208,274 -> 263,105
  (road city-1-loc-1 city-1-loc-8)
  (= (road-length city-1-loc-1 city-1-loc-8) 18)
  (= (fuel-demand city-1-loc-1 city-1-loc-8) 36)
  ; 263,105 -> 512,78
  (road city-1-loc-8 city-1-loc-3)
  (= (road-length city-1-loc-8 city-1-loc-3) 25)
  (= (fuel-demand city-1-loc-8 city-1-loc-3) 50)
  ; 512,78 -> 263,105
  (road city-1-loc-3 city-1-loc-8)
  (= (road-length city-1-loc-3 city-1-loc-8) 25)
  (= (fuel-demand city-1-loc-3 city-1-loc-8) 50)
  ; 263,105 -> 35,217
  (road city-1-loc-8 city-1-loc-5)
  (= (road-length city-1-loc-8 city-1-loc-5) 26)
  (= (fuel-demand city-1-loc-8 city-1-loc-5) 51)
  ; 35,217 -> 263,105
  (road city-1-loc-5 city-1-loc-8)
  (= (road-length city-1-loc-5 city-1-loc-8) 26)
  (= (fuel-demand city-1-loc-5 city-1-loc-8) 51)
  ; 127,449 -> 208,274
  (road city-1-loc-9 city-1-loc-1)
  (= (road-length city-1-loc-9 city-1-loc-1) 20)
  (= (fuel-demand city-1-loc-9 city-1-loc-1) 39)
  ; 208,274 -> 127,449
  (road city-1-loc-1 city-1-loc-9)
  (= (road-length city-1-loc-1 city-1-loc-9) 20)
  (= (fuel-demand city-1-loc-1 city-1-loc-9) 39)
  ; 127,449 -> 35,217
  (road city-1-loc-9 city-1-loc-5)
  (= (road-length city-1-loc-9 city-1-loc-5) 25)
  (= (fuel-demand city-1-loc-9 city-1-loc-5) 50)
  ; 35,217 -> 127,449
  (road city-1-loc-5 city-1-loc-9)
  (= (road-length city-1-loc-5 city-1-loc-9) 25)
  (= (fuel-demand city-1-loc-5 city-1-loc-9) 50)
  ; 1679,346 -> 1740,297
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 8)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 16)
  ; 1740,297 -> 1679,346
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 8)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 16)
  ; 1697,501 -> 1740,297
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 21)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 42)
  ; 1740,297 -> 1697,501
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 21)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 42)
  ; 1697,501 -> 1679,346
  (road city-2-loc-3 city-2-loc-2)
  (= (road-length city-2-loc-3 city-2-loc-2) 16)
  (= (fuel-demand city-2-loc-3 city-2-loc-2) 32)
  ; 1679,346 -> 1697,501
  (road city-2-loc-2 city-2-loc-3)
  (= (road-length city-2-loc-2 city-2-loc-3) 16)
  (= (fuel-demand city-2-loc-2 city-2-loc-3) 32)
  ; 1857,279 -> 1740,297
  (road city-2-loc-4 city-2-loc-1)
  (= (road-length city-2-loc-4 city-2-loc-1) 12)
  (= (fuel-demand city-2-loc-4 city-2-loc-1) 24)
  ; 1740,297 -> 1857,279
  (road city-2-loc-1 city-2-loc-4)
  (= (road-length city-2-loc-1 city-2-loc-4) 12)
  (= (fuel-demand city-2-loc-1 city-2-loc-4) 24)
  ; 1857,279 -> 1679,346
  (road city-2-loc-4 city-2-loc-2)
  (= (road-length city-2-loc-4 city-2-loc-2) 19)
  (= (fuel-demand city-2-loc-4 city-2-loc-2) 38)
  ; 1679,346 -> 1857,279
  (road city-2-loc-2 city-2-loc-4)
  (= (road-length city-2-loc-2 city-2-loc-4) 19)
  (= (fuel-demand city-2-loc-2 city-2-loc-4) 38)
  ; 1790,452 -> 1740,297
  (road city-2-loc-5 city-2-loc-1)
  (= (road-length city-2-loc-5 city-2-loc-1) 17)
  (= (fuel-demand city-2-loc-5 city-2-loc-1) 33)
  ; 1740,297 -> 1790,452
  (road city-2-loc-1 city-2-loc-5)
  (= (road-length city-2-loc-1 city-2-loc-5) 17)
  (= (fuel-demand city-2-loc-1 city-2-loc-5) 33)
  ; 1790,452 -> 1679,346
  (road city-2-loc-5 city-2-loc-2)
  (= (road-length city-2-loc-5 city-2-loc-2) 16)
  (= (fuel-demand city-2-loc-5 city-2-loc-2) 31)
  ; 1679,346 -> 1790,452
  (road city-2-loc-2 city-2-loc-5)
  (= (road-length city-2-loc-2 city-2-loc-5) 16)
  (= (fuel-demand city-2-loc-2 city-2-loc-5) 31)
  ; 1790,452 -> 1697,501
  (road city-2-loc-5 city-2-loc-3)
  (= (road-length city-2-loc-5 city-2-loc-3) 11)
  (= (fuel-demand city-2-loc-5 city-2-loc-3) 21)
  ; 1697,501 -> 1790,452
  (road city-2-loc-3 city-2-loc-5)
  (= (road-length city-2-loc-3 city-2-loc-5) 11)
  (= (fuel-demand city-2-loc-3 city-2-loc-5) 21)
  ; 1790,452 -> 1857,279
  (road city-2-loc-5 city-2-loc-4)
  (= (road-length city-2-loc-5 city-2-loc-4) 19)
  (= (fuel-demand city-2-loc-5 city-2-loc-4) 38)
  ; 1857,279 -> 1790,452
  (road city-2-loc-4 city-2-loc-5)
  (= (road-length city-2-loc-4 city-2-loc-5) 19)
  (= (fuel-demand city-2-loc-4 city-2-loc-5) 38)
  ; 1516,330 -> 1740,297
  (road city-2-loc-6 city-2-loc-1)
  (= (road-length city-2-loc-6 city-2-loc-1) 23)
  (= (fuel-demand city-2-loc-6 city-2-loc-1) 46)
  ; 1740,297 -> 1516,330
  (road city-2-loc-1 city-2-loc-6)
  (= (road-length city-2-loc-1 city-2-loc-6) 23)
  (= (fuel-demand city-2-loc-1 city-2-loc-6) 46)
  ; 1516,330 -> 1679,346
  (road city-2-loc-6 city-2-loc-2)
  (= (road-length city-2-loc-6 city-2-loc-2) 17)
  (= (fuel-demand city-2-loc-6 city-2-loc-2) 33)
  ; 1679,346 -> 1516,330
  (road city-2-loc-2 city-2-loc-6)
  (= (road-length city-2-loc-2 city-2-loc-6) 17)
  (= (fuel-demand city-2-loc-2 city-2-loc-6) 33)
  ; 1516,330 -> 1697,501
  (road city-2-loc-6 city-2-loc-3)
  (= (road-length city-2-loc-6 city-2-loc-3) 25)
  (= (fuel-demand city-2-loc-6 city-2-loc-3) 50)
  ; 1697,501 -> 1516,330
  (road city-2-loc-3 city-2-loc-6)
  (= (road-length city-2-loc-3 city-2-loc-6) 25)
  (= (fuel-demand city-2-loc-3 city-2-loc-6) 50)
  ; 1572,692 -> 1697,501
  (road city-2-loc-7 city-2-loc-3)
  (= (road-length city-2-loc-7 city-2-loc-3) 23)
  (= (fuel-demand city-2-loc-7 city-2-loc-3) 46)
  ; 1697,501 -> 1572,692
  (road city-2-loc-3 city-2-loc-7)
  (= (road-length city-2-loc-3 city-2-loc-7) 23)
  (= (fuel-demand city-2-loc-3 city-2-loc-7) 46)
  ; 1432,519 -> 1697,501
  (road city-2-loc-8 city-2-loc-3)
  (= (road-length city-2-loc-8 city-2-loc-3) 27)
  (= (fuel-demand city-2-loc-8 city-2-loc-3) 54)
  ; 1697,501 -> 1432,519
  (road city-2-loc-3 city-2-loc-8)
  (= (road-length city-2-loc-3 city-2-loc-8) 27)
  (= (fuel-demand city-2-loc-3 city-2-loc-8) 54)
  ; 1432,519 -> 1516,330
  (road city-2-loc-8 city-2-loc-6)
  (= (road-length city-2-loc-8 city-2-loc-6) 21)
  (= (fuel-demand city-2-loc-8 city-2-loc-6) 42)
  ; 1516,330 -> 1432,519
  (road city-2-loc-6 city-2-loc-8)
  (= (road-length city-2-loc-6 city-2-loc-8) 21)
  (= (fuel-demand city-2-loc-6 city-2-loc-8) 42)
  ; 1432,519 -> 1572,692
  (road city-2-loc-8 city-2-loc-7)
  (= (road-length city-2-loc-8 city-2-loc-7) 23)
  (= (fuel-demand city-2-loc-8 city-2-loc-7) 45)
  ; 1572,692 -> 1432,519
  (road city-2-loc-7 city-2-loc-8)
  (= (road-length city-2-loc-7 city-2-loc-8) 23)
  (= (fuel-demand city-2-loc-7 city-2-loc-8) 45)
  ; 2030,536 -> 1790,452
  (road city-2-loc-9 city-2-loc-5)
  (= (road-length city-2-loc-9 city-2-loc-5) 26)
  (= (fuel-demand city-2-loc-9 city-2-loc-5) 51)
  ; 1790,452 -> 2030,536
  (road city-2-loc-5 city-2-loc-9)
  (= (road-length city-2-loc-5 city-2-loc-9) 26)
  (= (fuel-demand city-2-loc-5 city-2-loc-9) 51)
  ; 606,306 <-> 1432,519
  (road city-1-loc-2 city-2-loc-8)
  (= (road-length city-1-loc-2 city-2-loc-8) 86)
  (= (fuel-demand city-1-loc-2 city-2-loc-8) 43)
  (road city-2-loc-8 city-1-loc-2)
  (= (road-length city-2-loc-8 city-1-loc-2) 86)
  (= (fuel-demand city-2-loc-8 city-1-loc-2) 43)
  (has-petrol-station city-1-loc-2)
  (has-petrol-station city-2-loc-8)
  (at package-1 city-1-loc-1)
  (at package-2 city-1-loc-2)
  (at package-3 city-1-loc-8)
  (at package-4 city-1-loc-8)
  (at package-5 city-1-loc-5)
  (at truck-1 city-2-loc-3)
  (= (fuel-left truck-1) 639)
  (= (fuel-max truck-1) 639)
  (capacity truck-1 capacity-3)
  (at truck-2 city-2-loc-7)
  (= (fuel-left truck-2) 639)
  (= (fuel-max truck-2) 639)
  (capacity truck-2 capacity-4)
  (at truck-3 city-2-loc-6)
  (= (fuel-left truck-3) 639)
  (= (fuel-max truck-3) 639)
  (capacity truck-3 capacity-2)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-6))
  (preference delivery-2 (at package-2 city-2-loc-4))
  (preference delivery-3 (at package-3 city-2-loc-8))
  (preference delivery-4 (at package-4 city-2-loc-2))
  (preference delivery-5 (at package-5 city-2-loc-9))
 ))
 (:metric maximize
   (- 1245
     (+ (total-cost)
       (* (is-violated delivery-1) 215)
       (* (is-violated delivery-2) 251)
       (* (is-violated delivery-3) 276)
       (* (is-violated delivery-4) 252)
       (* (is-violated delivery-5) 251)
     )
   )
 )
)
