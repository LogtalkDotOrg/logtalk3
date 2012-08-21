; Transport city-netbenefit-1petrol-station-9nodes-1000size-3degree-100mindistance-2trucks-3packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-9nodes-1000size-3degree-100mindistance-2trucks-3packagespercity-4016seed)
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
  city-loc-9 - location
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
  ; 374,712 -> 421,387
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 33)
  (= (fuel-demand city-loc-4 city-loc-1) 66)
  ; 421,387 -> 374,712
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 33)
  (= (fuel-demand city-loc-1 city-loc-4) 66)
  ; 436,887 -> 374,712
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 19)
  (= (fuel-demand city-loc-5 city-loc-4) 38)
  ; 374,712 -> 436,887
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 19)
  (= (fuel-demand city-loc-4 city-loc-5) 38)
  ; 825,140 -> 618,9
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 25)
  (= (fuel-demand city-loc-6 city-loc-2) 49)
  ; 618,9 -> 825,140
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 25)
  (= (fuel-demand city-loc-2 city-loc-6) 49)
  ; 560,280 -> 421,387
  (road city-loc-7 city-loc-1)
  (= (road-length city-loc-7 city-loc-1) 18)
  (= (fuel-demand city-loc-7 city-loc-1) 35)
  ; 421,387 -> 560,280
  (road city-loc-1 city-loc-7)
  (= (road-length city-loc-1 city-loc-7) 18)
  (= (fuel-demand city-loc-1 city-loc-7) 35)
  ; 560,280 -> 618,9
  (road city-loc-7 city-loc-2)
  (= (road-length city-loc-7 city-loc-2) 28)
  (= (fuel-demand city-loc-7 city-loc-2) 56)
  ; 618,9 -> 560,280
  (road city-loc-2 city-loc-7)
  (= (road-length city-loc-2 city-loc-7) 28)
  (= (fuel-demand city-loc-2 city-loc-7) 56)
  ; 560,280 -> 825,140
  (road city-loc-7 city-loc-6)
  (= (road-length city-loc-7 city-loc-6) 30)
  (= (fuel-demand city-loc-7 city-loc-6) 60)
  ; 825,140 -> 560,280
  (road city-loc-6 city-loc-7)
  (= (road-length city-loc-6 city-loc-7) 30)
  (= (fuel-demand city-loc-6 city-loc-7) 60)
  ; 458,249 -> 421,387
  (road city-loc-8 city-loc-1)
  (= (road-length city-loc-8 city-loc-1) 15)
  (= (fuel-demand city-loc-8 city-loc-1) 29)
  ; 421,387 -> 458,249
  (road city-loc-1 city-loc-8)
  (= (road-length city-loc-1 city-loc-8) 15)
  (= (fuel-demand city-loc-1 city-loc-8) 29)
  ; 458,249 -> 618,9
  (road city-loc-8 city-loc-2)
  (= (road-length city-loc-8 city-loc-2) 29)
  (= (fuel-demand city-loc-8 city-loc-2) 58)
  ; 618,9 -> 458,249
  (road city-loc-2 city-loc-8)
  (= (road-length city-loc-2 city-loc-8) 29)
  (= (fuel-demand city-loc-2 city-loc-8) 58)
  ; 458,249 -> 207,43
  (road city-loc-8 city-loc-3)
  (= (road-length city-loc-8 city-loc-3) 33)
  (= (fuel-demand city-loc-8 city-loc-3) 65)
  ; 207,43 -> 458,249
  (road city-loc-3 city-loc-8)
  (= (road-length city-loc-3 city-loc-8) 33)
  (= (fuel-demand city-loc-3 city-loc-8) 65)
  ; 458,249 -> 825,140
  (road city-loc-8 city-loc-6)
  (= (road-length city-loc-8 city-loc-6) 39)
  (= (fuel-demand city-loc-8 city-loc-6) 77)
  ; 825,140 -> 458,249
  (road city-loc-6 city-loc-8)
  (= (road-length city-loc-6 city-loc-8) 39)
  (= (fuel-demand city-loc-6 city-loc-8) 77)
  ; 458,249 -> 560,280
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 11)
  (= (fuel-demand city-loc-8 city-loc-7) 22)
  ; 560,280 -> 458,249
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 11)
  (= (fuel-demand city-loc-7 city-loc-8) 22)
  ; 444,484 -> 421,387
  (road city-loc-9 city-loc-1)
  (= (road-length city-loc-9 city-loc-1) 10)
  (= (fuel-demand city-loc-9 city-loc-1) 20)
  ; 421,387 -> 444,484
  (road city-loc-1 city-loc-9)
  (= (road-length city-loc-1 city-loc-9) 10)
  (= (fuel-demand city-loc-1 city-loc-9) 20)
  ; 444,484 -> 374,712
  (road city-loc-9 city-loc-4)
  (= (road-length city-loc-9 city-loc-4) 24)
  (= (fuel-demand city-loc-9 city-loc-4) 48)
  ; 374,712 -> 444,484
  (road city-loc-4 city-loc-9)
  (= (road-length city-loc-4 city-loc-9) 24)
  (= (fuel-demand city-loc-4 city-loc-9) 48)
  ; 444,484 -> 560,280
  (road city-loc-9 city-loc-7)
  (= (road-length city-loc-9 city-loc-7) 24)
  (= (fuel-demand city-loc-9 city-loc-7) 47)
  ; 560,280 -> 444,484
  (road city-loc-7 city-loc-9)
  (= (road-length city-loc-7 city-loc-9) 24)
  (= (fuel-demand city-loc-7 city-loc-9) 47)
  ; 444,484 -> 458,249
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 24)
  (= (fuel-demand city-loc-9 city-loc-8) 47)
  ; 458,249 -> 444,484
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 24)
  (= (fuel-demand city-loc-8 city-loc-9) 47)
  (at package-1 city-loc-2)
  (at package-2 city-loc-3)
  (at package-3 city-loc-3)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-6)
  (capacity truck-1 capacity-4)
  (= (fuel-left truck-1) 226)
  (= (fuel-max truck-1) 226)
  (at truck-2 city-loc-3)
  (capacity truck-2 capacity-3)
  (= (fuel-left truck-2) 226)
  (= (fuel-max truck-2) 226)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-4))
  (preference delivery-2 (at package-2 city-loc-1))
  (preference delivery-3 (at package-3 city-loc-1))
 ))
 (:metric maximize
   (- 253
     (+ (total-cost)
       (* (is-violated delivery-1) 58)
       (* (is-violated delivery-2) 85)
       (* (is-violated delivery-3) 110)
     )
   )
 )
)
