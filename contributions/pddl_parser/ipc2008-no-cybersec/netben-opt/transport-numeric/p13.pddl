; Transport city-netbenefit-1petrol-station-8nodes-1000size-3degree-100mindistance-2trucks-3packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-8nodes-1000size-3degree-100mindistance-2trucks-3packagespercity-4016seed)
 (:domain transport)
 (:objects
  city-loc-1 - location
  city-loc-2 - location
  city-loc-3 - location
  city-loc-4 - location
  city-loc-5 - location
  city-loc-6 - location
  city-loc-7 - location
  city-loc-8 - location
  truck-1 - vehicle
  truck-2 - vehicle
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
  ; 256,440 -> 53,545
  (road city-loc-2 city-loc-1)
  (= (road-length city-loc-2 city-loc-1) 23)
  (= (fuel-demand city-loc-2 city-loc-1) 46)
  ; 53,545 -> 256,440
  (road city-loc-1 city-loc-2)
  (= (road-length city-loc-1 city-loc-2) 23)
  (= (fuel-demand city-loc-1 city-loc-2) 46)
  ; 536,738 -> 256,440
  (road city-loc-3 city-loc-2)
  (= (road-length city-loc-3 city-loc-2) 41)
  (= (fuel-demand city-loc-3 city-loc-2) 82)
  ; 256,440 -> 536,738
  (road city-loc-2 city-loc-3)
  (= (road-length city-loc-2 city-loc-3) 41)
  (= (fuel-demand city-loc-2 city-loc-3) 82)
  ; 421,387 -> 53,545
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 40)
  (= (fuel-demand city-loc-4 city-loc-1) 80)
  ; 53,545 -> 421,387
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 40)
  (= (fuel-demand city-loc-1 city-loc-4) 80)
  ; 421,387 -> 256,440
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 18)
  (= (fuel-demand city-loc-4 city-loc-2) 35)
  ; 256,440 -> 421,387
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 18)
  (= (fuel-demand city-loc-2 city-loc-4) 35)
  ; 421,387 -> 536,738
  (road city-loc-4 city-loc-3)
  (= (road-length city-loc-4 city-loc-3) 37)
  (= (fuel-demand city-loc-4 city-loc-3) 74)
  ; 536,738 -> 421,387
  (road city-loc-3 city-loc-4)
  (= (road-length city-loc-3 city-loc-4) 37)
  (= (fuel-demand city-loc-3 city-loc-4) 74)
  ; 207,43 -> 256,440
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 40)
  (= (fuel-demand city-loc-6 city-loc-2) 80)
  ; 256,440 -> 207,43
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 40)
  (= (fuel-demand city-loc-2 city-loc-6) 80)
  ; 207,43 -> 421,387
  (road city-loc-6 city-loc-4)
  (= (road-length city-loc-6 city-loc-4) 41)
  (= (fuel-demand city-loc-6 city-loc-4) 81)
  ; 421,387 -> 207,43
  (road city-loc-4 city-loc-6)
  (= (road-length city-loc-4 city-loc-6) 41)
  (= (fuel-demand city-loc-4 city-loc-6) 81)
  ; 207,43 -> 618,9
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 42)
  (= (fuel-demand city-loc-6 city-loc-5) 83)
  ; 618,9 -> 207,43
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 42)
  (= (fuel-demand city-loc-5 city-loc-6) 83)
  ; 374,712 -> 53,545
  (road city-loc-7 city-loc-1)
  (= (road-length city-loc-7 city-loc-1) 37)
  (= (fuel-demand city-loc-7 city-loc-1) 73)
  ; 53,545 -> 374,712
  (road city-loc-1 city-loc-7)
  (= (road-length city-loc-1 city-loc-7) 37)
  (= (fuel-demand city-loc-1 city-loc-7) 73)
  ; 374,712 -> 256,440
  (road city-loc-7 city-loc-2)
  (= (road-length city-loc-7 city-loc-2) 30)
  (= (fuel-demand city-loc-7 city-loc-2) 60)
  ; 256,440 -> 374,712
  (road city-loc-2 city-loc-7)
  (= (road-length city-loc-2 city-loc-7) 30)
  (= (fuel-demand city-loc-2 city-loc-7) 60)
  ; 374,712 -> 536,738
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 17)
  (= (fuel-demand city-loc-7 city-loc-3) 33)
  ; 536,738 -> 374,712
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 17)
  (= (fuel-demand city-loc-3 city-loc-7) 33)
  ; 374,712 -> 421,387
  (road city-loc-7 city-loc-4)
  (= (road-length city-loc-7 city-loc-4) 33)
  (= (fuel-demand city-loc-7 city-loc-4) 66)
  ; 421,387 -> 374,712
  (road city-loc-4 city-loc-7)
  (= (road-length city-loc-4 city-loc-7) 33)
  (= (fuel-demand city-loc-4 city-loc-7) 66)
  ; 436,887 -> 536,738
  (road city-loc-8 city-loc-3)
  (= (road-length city-loc-8 city-loc-3) 18)
  (= (fuel-demand city-loc-8 city-loc-3) 36)
  ; 536,738 -> 436,887
  (road city-loc-3 city-loc-8)
  (= (road-length city-loc-3 city-loc-8) 18)
  (= (fuel-demand city-loc-3 city-loc-8) 36)
  ; 436,887 -> 374,712
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 19)
  (= (fuel-demand city-loc-8 city-loc-7) 38)
  ; 374,712 -> 436,887
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 19)
  (= (fuel-demand city-loc-7 city-loc-8) 38)
  (at package-1 city-loc-7)
  (at package-2 city-loc-2)
  (at package-3 city-loc-3)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-5)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 458)
  (= (fuel-max truck-1) 458)
  (at truck-2 city-loc-4)
  (capacity truck-2 capacity-2)
  (= (fuel-left truck-2) 458)
  (= (fuel-max truck-2) 458)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-2))
  (preference delivery-2 (at package-2 city-loc-4))
  (preference delivery-3 (at package-3 city-loc-7))
 ))
 (:metric maximize
   (- 296
     (+ (total-cost)
       (* (is-violated delivery-1) 129)
       (* (is-violated delivery-2) 64)
       (* (is-violated delivery-3) 103)
     )
   )
 )
)
