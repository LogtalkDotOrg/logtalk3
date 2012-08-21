; Transport two-cities-netbenefit-4nodes-700size-3degree-70mindistance-1trucks-2packages-6024seed

(define (problem transport-two-cities-netbenefit-4nodes-700size-3degree-70mindistance-1trucks-2packages-6024seed)
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
  truck-1 - vehicle
  package-1 - package
  package-2 - package
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
  ; 208,274 -> 385,630
  (road city-1-loc-2 city-1-loc-1)
  (= (road-length city-1-loc-2 city-1-loc-1) 40)
  (= (fuel-demand city-1-loc-2 city-1-loc-1) 80)
  ; 385,630 -> 208,274
  (road city-1-loc-1 city-1-loc-2)
  (= (road-length city-1-loc-1 city-1-loc-2) 40)
  (= (fuel-demand city-1-loc-1 city-1-loc-2) 80)
  ; 606,306 -> 385,630
  (road city-1-loc-3 city-1-loc-1)
  (= (road-length city-1-loc-3 city-1-loc-1) 40)
  (= (fuel-demand city-1-loc-3 city-1-loc-1) 79)
  ; 385,630 -> 606,306
  (road city-1-loc-1 city-1-loc-3)
  (= (road-length city-1-loc-1 city-1-loc-3) 40)
  (= (fuel-demand city-1-loc-1 city-1-loc-3) 79)
  ; 606,306 -> 208,274
  (road city-1-loc-3 city-1-loc-2)
  (= (road-length city-1-loc-3 city-1-loc-2) 40)
  (= (fuel-demand city-1-loc-3 city-1-loc-2) 80)
  ; 208,274 -> 606,306
  (road city-1-loc-2 city-1-loc-3)
  (= (road-length city-1-loc-2 city-1-loc-3) 40)
  (= (fuel-demand city-1-loc-2 city-1-loc-3) 80)
  ; 512,78 -> 208,274
  (road city-1-loc-4 city-1-loc-2)
  (= (road-length city-1-loc-4 city-1-loc-2) 37)
  (= (fuel-demand city-1-loc-4 city-1-loc-2) 73)
  ; 208,274 -> 512,78
  (road city-1-loc-2 city-1-loc-4)
  (= (road-length city-1-loc-2 city-1-loc-4) 37)
  (= (fuel-demand city-1-loc-2 city-1-loc-4) 73)
  ; 512,78 -> 606,306
  (road city-1-loc-4 city-1-loc-3)
  (= (road-length city-1-loc-4 city-1-loc-3) 25)
  (= (fuel-demand city-1-loc-4 city-1-loc-3) 50)
  ; 606,306 -> 512,78
  (road city-1-loc-3 city-1-loc-4)
  (= (road-length city-1-loc-3 city-1-loc-4) 25)
  (= (fuel-demand city-1-loc-3 city-1-loc-4) 50)
  ; 1527,449 -> 1663,105
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 37)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 74)
  ; 1663,105 -> 1527,449
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 37)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 74)
  ; 1740,297 -> 1663,105
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 21)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 42)
  ; 1663,105 -> 1740,297
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 21)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 42)
  ; 1740,297 -> 1527,449
  (road city-2-loc-3 city-2-loc-2)
  (= (road-length city-2-loc-3 city-2-loc-2) 27)
  (= (fuel-demand city-2-loc-3 city-2-loc-2) 53)
  ; 1527,449 -> 1740,297
  (road city-2-loc-2 city-2-loc-3)
  (= (road-length city-2-loc-2 city-2-loc-3) 27)
  (= (fuel-demand city-2-loc-2 city-2-loc-3) 53)
  ; 1679,346 -> 1663,105
  (road city-2-loc-4 city-2-loc-1)
  (= (road-length city-2-loc-4 city-2-loc-1) 25)
  (= (fuel-demand city-2-loc-4 city-2-loc-1) 49)
  ; 1663,105 -> 1679,346
  (road city-2-loc-1 city-2-loc-4)
  (= (road-length city-2-loc-1 city-2-loc-4) 25)
  (= (fuel-demand city-2-loc-1 city-2-loc-4) 49)
  ; 1679,346 -> 1527,449
  (road city-2-loc-4 city-2-loc-2)
  (= (road-length city-2-loc-4 city-2-loc-2) 19)
  (= (fuel-demand city-2-loc-4 city-2-loc-2) 37)
  ; 1527,449 -> 1679,346
  (road city-2-loc-2 city-2-loc-4)
  (= (road-length city-2-loc-2 city-2-loc-4) 19)
  (= (fuel-demand city-2-loc-2 city-2-loc-4) 37)
  ; 1679,346 -> 1740,297
  (road city-2-loc-4 city-2-loc-3)
  (= (road-length city-2-loc-4 city-2-loc-3) 8)
  (= (fuel-demand city-2-loc-4 city-2-loc-3) 16)
  ; 1740,297 -> 1679,346
  (road city-2-loc-3 city-2-loc-4)
  (= (road-length city-2-loc-3 city-2-loc-4) 8)
  (= (fuel-demand city-2-loc-3 city-2-loc-4) 16)
  ; 606,306 <-> 1527,449
  (road city-1-loc-3 city-2-loc-2)
  (= (road-length city-1-loc-3 city-2-loc-2) 94)
  (= (fuel-demand city-1-loc-3 city-2-loc-2) 47)
  (road city-2-loc-2 city-1-loc-3)
  (= (road-length city-2-loc-2 city-1-loc-3) 94)
  (= (fuel-demand city-2-loc-2 city-1-loc-3) 47)
  (has-petrol-station city-1-loc-3)
  (has-petrol-station city-2-loc-2)
  (at package-1 city-1-loc-2)
  (at package-2 city-1-loc-3)
  (at truck-1 city-2-loc-3)
  (= (fuel-left truck-1) 699)
  (= (fuel-max truck-1) 699)
  (capacity truck-1 capacity-3)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-3))
  (preference delivery-2 (at package-2 city-2-loc-3))
 ))
 (:metric maximize
   (- 563
     (+ (total-cost)
       (* (is-violated delivery-1) 266)
       (* (is-violated delivery-2) 297)
     )
   )
 )
)
