; Transport two-cities-netbenefit-3nodes-700size-3degree-70mindistance-1trucks-2packages-6024seed

(define (problem transport-two-cities-netbenefit-3nodes-700size-3degree-70mindistance-1trucks-2packages-6024seed)
 (:domain transport)
 (:objects
  city-1-loc-1 - location
  city-2-loc-1 - location
  city-1-loc-2 - location
  city-2-loc-2 - location
  city-1-loc-3 - location
  city-2-loc-3 - location
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
  ; 646,343 -> 288,57
  (road city-1-loc-2 city-1-loc-1)
  (= (road-length city-1-loc-2 city-1-loc-1) 46)
  (= (fuel-demand city-1-loc-2 city-1-loc-1) 92)
  ; 288,57 -> 646,343
  (road city-1-loc-1 city-1-loc-2)
  (= (road-length city-1-loc-1 city-1-loc-2) 46)
  (= (fuel-demand city-1-loc-1 city-1-loc-2) 92)
  ; 82,330 -> 288,57
  (road city-1-loc-3 city-1-loc-1)
  (= (road-length city-1-loc-3 city-1-loc-1) 35)
  (= (fuel-demand city-1-loc-3 city-1-loc-1) 69)
  ; 288,57 -> 82,330
  (road city-1-loc-1 city-1-loc-3)
  (= (road-length city-1-loc-1 city-1-loc-3) 35)
  (= (fuel-demand city-1-loc-1 city-1-loc-3) 69)
  ; 1955,96 -> 1909,220
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 14)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 27)
  ; 1909,220 -> 1955,96
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 14)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 27)
  ; 1785,630 -> 1909,220
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 43)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 86)
  ; 1909,220 -> 1785,630
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 43)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 86)
  ; 646,343 <-> 1785,630
  (road city-1-loc-2 city-2-loc-3)
  (= (road-length city-1-loc-2 city-2-loc-3) 118)
  (= (fuel-demand city-1-loc-2 city-2-loc-3) 59)
  (road city-2-loc-3 city-1-loc-2)
  (= (road-length city-2-loc-3 city-1-loc-2) 118)
  (= (fuel-demand city-2-loc-3 city-1-loc-2) 59)
  (has-petrol-station city-1-loc-2)
  (has-petrol-station city-2-loc-3)
  (at package-1 city-1-loc-1)
  (at package-2 city-1-loc-2)
  (at truck-1 city-2-loc-3)
  (= (fuel-left truck-1) 880)
  (= (fuel-max truck-1) 880)
  (capacity truck-1 capacity-3)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-3))
  (preference delivery-2 (at package-2 city-2-loc-1))
 ))
 (:metric maximize
   (- 614
     (+ (total-cost)
       (* (is-violated delivery-1) 333)
       (* (is-violated delivery-2) 281)
     )
   )
 )
)
