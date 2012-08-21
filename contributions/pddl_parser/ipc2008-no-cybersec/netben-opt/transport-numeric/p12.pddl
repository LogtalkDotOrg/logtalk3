; Transport city-netbenefit-1petrol-station-7nodes-1000size-3degree-100mindistance-2trucks-2packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-7nodes-1000size-3degree-100mindistance-2trucks-2packagespercity-4016seed)
 (:domain transport)
 (:objects
  city-loc-1 - location
  city-loc-2 - location
  city-loc-3 - location
  city-loc-4 - location
  city-loc-5 - location
  city-loc-6 - location
  city-loc-7 - location
  truck-1 - vehicle
  truck-2 - vehicle
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
  ; 256,440 -> 26,577
  (road city-loc-3 city-loc-1)
  (= (road-length city-loc-3 city-loc-1) 27)
  (= (fuel-demand city-loc-3 city-loc-1) 54)
  ; 26,577 -> 256,440
  (road city-loc-1 city-loc-3)
  (= (road-length city-loc-1 city-loc-3) 27)
  (= (fuel-demand city-loc-1 city-loc-3) 54)
  ; 536,738 -> 256,440
  (road city-loc-4 city-loc-3)
  (= (road-length city-loc-4 city-loc-3) 41)
  (= (fuel-demand city-loc-4 city-loc-3) 82)
  ; 256,440 -> 536,738
  (road city-loc-3 city-loc-4)
  (= (road-length city-loc-3 city-loc-4) 41)
  (= (fuel-demand city-loc-3 city-loc-4) 82)
  ; 421,387 -> 26,577
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 44)
  (= (fuel-demand city-loc-5 city-loc-1) 88)
  ; 26,577 -> 421,387
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 44)
  (= (fuel-demand city-loc-1 city-loc-5) 88)
  ; 421,387 -> 256,440
  (road city-loc-5 city-loc-3)
  (= (road-length city-loc-5 city-loc-3) 18)
  (= (fuel-demand city-loc-5 city-loc-3) 35)
  ; 256,440 -> 421,387
  (road city-loc-3 city-loc-5)
  (= (road-length city-loc-3 city-loc-5) 18)
  (= (fuel-demand city-loc-3 city-loc-5) 35)
  ; 421,387 -> 536,738
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 37)
  (= (fuel-demand city-loc-5 city-loc-4) 74)
  ; 536,738 -> 421,387
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 37)
  (= (fuel-demand city-loc-4 city-loc-5) 74)
  ; 618,9 -> 839,137
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 26)
  (= (fuel-demand city-loc-6 city-loc-2) 51)
  ; 839,137 -> 618,9
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 26)
  (= (fuel-demand city-loc-2 city-loc-6) 51)
  ; 618,9 -> 421,387
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 43)
  (= (fuel-demand city-loc-6 city-loc-5) 86)
  ; 421,387 -> 618,9
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 43)
  (= (fuel-demand city-loc-5 city-loc-6) 86)
  ; 207,43 -> 256,440
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 40)
  (= (fuel-demand city-loc-7 city-loc-3) 80)
  ; 256,440 -> 207,43
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 40)
  (= (fuel-demand city-loc-3 city-loc-7) 80)
  ; 207,43 -> 421,387
  (road city-loc-7 city-loc-5)
  (= (road-length city-loc-7 city-loc-5) 41)
  (= (fuel-demand city-loc-7 city-loc-5) 81)
  ; 421,387 -> 207,43
  (road city-loc-5 city-loc-7)
  (= (road-length city-loc-5 city-loc-7) 41)
  (= (fuel-demand city-loc-5 city-loc-7) 81)
  ; 207,43 -> 618,9
  (road city-loc-7 city-loc-6)
  (= (road-length city-loc-7 city-loc-6) 42)
  (= (fuel-demand city-loc-7 city-loc-6) 83)
  ; 618,9 -> 207,43
  (road city-loc-6 city-loc-7)
  (= (road-length city-loc-6 city-loc-7) 42)
  (= (fuel-demand city-loc-6 city-loc-7) 83)
  (at package-1 city-loc-3)
  (at package-2 city-loc-5)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-7)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 492)
  (= (fuel-max truck-1) 492)
  (at truck-2 city-loc-1)
  (capacity truck-2 capacity-4)
  (= (fuel-left truck-2) 492)
  (= (fuel-max truck-2) 492)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-4))
  (preference delivery-2 (at package-2 city-loc-2))
 ))
 (:metric maximize
   (- 171
     (+ (total-cost)
       (* (is-violated delivery-1) 96)
       (* (is-violated delivery-2) 75)
     )
   )
 )
)
