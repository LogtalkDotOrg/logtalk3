; Transport two-cities-netbenefit-5nodes-700size-3degree-70mindistance-1trucks-3packages-6024seed

(define (problem transport-two-cities-netbenefit-5nodes-700size-3degree-70mindistance-1trucks-3packages-6024seed)
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
  truck-1 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
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
  ; 127,449 -> 390,532
  (road city-1-loc-3 city-1-loc-1)
  (= (road-length city-1-loc-3 city-1-loc-1) 28)
  (= (fuel-demand city-1-loc-3 city-1-loc-1) 56)
  ; 390,532 -> 127,449
  (road city-1-loc-1 city-1-loc-3)
  (= (road-length city-1-loc-1 city-1-loc-3) 28)
  (= (fuel-demand city-1-loc-1 city-1-loc-3) 56)
  ; 340,297 -> 390,532
  (road city-1-loc-4 city-1-loc-1)
  (= (road-length city-1-loc-4 city-1-loc-1) 24)
  (= (fuel-demand city-1-loc-4 city-1-loc-1) 48)
  ; 390,532 -> 340,297
  (road city-1-loc-1 city-1-loc-4)
  (= (road-length city-1-loc-1 city-1-loc-4) 24)
  (= (fuel-demand city-1-loc-1 city-1-loc-4) 48)
  ; 340,297 -> 263,105
  (road city-1-loc-4 city-1-loc-2)
  (= (road-length city-1-loc-4 city-1-loc-2) 21)
  (= (fuel-demand city-1-loc-4 city-1-loc-2) 42)
  ; 263,105 -> 340,297
  (road city-1-loc-2 city-1-loc-4)
  (= (road-length city-1-loc-2 city-1-loc-4) 21)
  (= (fuel-demand city-1-loc-2 city-1-loc-4) 42)
  ; 340,297 -> 127,449
  (road city-1-loc-4 city-1-loc-3)
  (= (road-length city-1-loc-4 city-1-loc-3) 27)
  (= (fuel-demand city-1-loc-4 city-1-loc-3) 53)
  ; 127,449 -> 340,297
  (road city-1-loc-3 city-1-loc-4)
  (= (road-length city-1-loc-3 city-1-loc-4) 27)
  (= (fuel-demand city-1-loc-3 city-1-loc-4) 53)
  ; 279,346 -> 390,532
  (road city-1-loc-5 city-1-loc-1)
  (= (road-length city-1-loc-5 city-1-loc-1) 22)
  (= (fuel-demand city-1-loc-5 city-1-loc-1) 44)
  ; 390,532 -> 279,346
  (road city-1-loc-1 city-1-loc-5)
  (= (road-length city-1-loc-1 city-1-loc-5) 22)
  (= (fuel-demand city-1-loc-1 city-1-loc-5) 44)
  ; 279,346 -> 263,105
  (road city-1-loc-5 city-1-loc-2)
  (= (road-length city-1-loc-5 city-1-loc-2) 25)
  (= (fuel-demand city-1-loc-5 city-1-loc-2) 49)
  ; 263,105 -> 279,346
  (road city-1-loc-2 city-1-loc-5)
  (= (road-length city-1-loc-2 city-1-loc-5) 25)
  (= (fuel-demand city-1-loc-2 city-1-loc-5) 49)
  ; 279,346 -> 127,449
  (road city-1-loc-5 city-1-loc-3)
  (= (road-length city-1-loc-5 city-1-loc-3) 19)
  (= (fuel-demand city-1-loc-5 city-1-loc-3) 37)
  ; 127,449 -> 279,346
  (road city-1-loc-3 city-1-loc-5)
  (= (road-length city-1-loc-3 city-1-loc-5) 19)
  (= (fuel-demand city-1-loc-3 city-1-loc-5) 37)
  ; 279,346 -> 340,297
  (road city-1-loc-5 city-1-loc-4)
  (= (road-length city-1-loc-5 city-1-loc-4) 8)
  (= (fuel-demand city-1-loc-5 city-1-loc-4) 16)
  ; 340,297 -> 279,346
  (road city-1-loc-4 city-1-loc-5)
  (= (road-length city-1-loc-4 city-1-loc-5) 8)
  (= (fuel-demand city-1-loc-4 city-1-loc-5) 16)
  ; 1857,279 -> 1697,501
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 28)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 55)
  ; 1697,501 -> 1857,279
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 28)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 55)
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
  ; 1516,330 -> 1857,279
  (road city-2-loc-4 city-2-loc-2)
  (= (road-length city-2-loc-4 city-2-loc-2) 35)
  (= (fuel-demand city-2-loc-4 city-2-loc-2) 69)
  ; 1857,279 -> 1516,330
  (road city-2-loc-2 city-2-loc-4)
  (= (road-length city-2-loc-2 city-2-loc-4) 35)
  (= (fuel-demand city-2-loc-2 city-2-loc-4) 69)
  ; 1516,330 -> 1790,452
  (road city-2-loc-4 city-2-loc-3)
  (= (road-length city-2-loc-4 city-2-loc-3) 30)
  (= (fuel-demand city-2-loc-4 city-2-loc-3) 60)
  ; 1790,452 -> 1516,330
  (road city-2-loc-3 city-2-loc-4)
  (= (road-length city-2-loc-3 city-2-loc-4) 30)
  (= (fuel-demand city-2-loc-3 city-2-loc-4) 60)
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
  ; 390,532 <-> 1516,330
  (road city-1-loc-1 city-2-loc-4)
  (= (road-length city-1-loc-1 city-2-loc-4) 115)
  (= (fuel-demand city-1-loc-1 city-2-loc-4) 58)
  (road city-2-loc-4 city-1-loc-1)
  (= (road-length city-2-loc-4 city-1-loc-1) 115)
  (= (fuel-demand city-2-loc-4 city-1-loc-1) 58)
  (has-petrol-station city-1-loc-1)
  (has-petrol-station city-2-loc-4)
  (at package-1 city-1-loc-2)
  (at package-2 city-1-loc-5)
  (at package-3 city-1-loc-1)
  (at truck-1 city-2-loc-4)
  (= (fuel-left truck-1) 857)
  (= (fuel-max truck-1) 857)
  (capacity truck-1 capacity-3)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-4))
  (preference delivery-2 (at package-2 city-2-loc-5))
  (preference delivery-3 (at package-3 city-2-loc-4))
 ))
 (:metric maximize
   (- 700
     (+ (total-cost)
       (* (is-violated delivery-1) 209)
       (* (is-violated delivery-2) 212)
       (* (is-violated delivery-3) 279)
     )
   )
 )
)
