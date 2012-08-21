; Transport city-netbenefit-1petrol-station-13nodes-1000size-3degree-100mindistance-2trucks-5packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-13nodes-1000size-3degree-100mindistance-2trucks-5packagespercity-4016seed)
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
  city-loc-10 - location
  city-loc-11 - location
  city-loc-12 - location
  city-loc-13 - location
  truck-1 - vehicle
  truck-2 - vehicle
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
  ; 256,440 -> 26,577
  (road city-loc-3 city-loc-1)
  (= (road-length city-loc-3 city-loc-1) 27)
  (= (fuel-demand city-loc-3 city-loc-1) 54)
  ; 26,577 -> 256,440
  (road city-loc-1 city-loc-3)
  (= (road-length city-loc-1 city-loc-3) 27)
  (= (fuel-demand city-loc-1 city-loc-3) 54)
  ; 421,387 -> 256,440
  (road city-loc-5 city-loc-3)
  (= (road-length city-loc-5 city-loc-3) 18)
  (= (fuel-demand city-loc-5 city-loc-3) 35)
  ; 256,440 -> 421,387
  (road city-loc-3 city-loc-5)
  (= (road-length city-loc-3 city-loc-5) 18)
  (= (fuel-demand city-loc-3 city-loc-5) 35)
  ; 618,9 -> 839,137
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 26)
  (= (fuel-demand city-loc-6 city-loc-2) 51)
  ; 839,137 -> 618,9
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 26)
  (= (fuel-demand city-loc-2 city-loc-6) 51)
  ; 374,712 -> 256,440
  (road city-loc-8 city-loc-3)
  (= (road-length city-loc-8 city-loc-3) 30)
  (= (fuel-demand city-loc-8 city-loc-3) 60)
  ; 256,440 -> 374,712
  (road city-loc-3 city-loc-8)
  (= (road-length city-loc-3 city-loc-8) 30)
  (= (fuel-demand city-loc-3 city-loc-8) 60)
  ; 374,712 -> 536,738
  (road city-loc-8 city-loc-4)
  (= (road-length city-loc-8 city-loc-4) 17)
  (= (fuel-demand city-loc-8 city-loc-4) 33)
  ; 536,738 -> 374,712
  (road city-loc-4 city-loc-8)
  (= (road-length city-loc-4 city-loc-8) 17)
  (= (fuel-demand city-loc-4 city-loc-8) 33)
  ; 436,887 -> 536,738
  (road city-loc-9 city-loc-4)
  (= (road-length city-loc-9 city-loc-4) 18)
  (= (fuel-demand city-loc-9 city-loc-4) 36)
  ; 536,738 -> 436,887
  (road city-loc-4 city-loc-9)
  (= (road-length city-loc-4 city-loc-9) 18)
  (= (fuel-demand city-loc-4 city-loc-9) 36)
  ; 436,887 -> 374,712
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 19)
  (= (fuel-demand city-loc-9 city-loc-8) 38)
  ; 374,712 -> 436,887
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 19)
  (= (fuel-demand city-loc-8 city-loc-9) 38)
  ; 560,280 -> 839,137
  (road city-loc-10 city-loc-2)
  (= (road-length city-loc-10 city-loc-2) 32)
  (= (fuel-demand city-loc-10 city-loc-2) 63)
  ; 839,137 -> 560,280
  (road city-loc-2 city-loc-10)
  (= (road-length city-loc-2 city-loc-10) 32)
  (= (fuel-demand city-loc-2 city-loc-10) 63)
  ; 560,280 -> 421,387
  (road city-loc-10 city-loc-5)
  (= (road-length city-loc-10 city-loc-5) 18)
  (= (fuel-demand city-loc-10 city-loc-5) 35)
  ; 421,387 -> 560,280
  (road city-loc-5 city-loc-10)
  (= (road-length city-loc-5 city-loc-10) 18)
  (= (fuel-demand city-loc-5 city-loc-10) 35)
  ; 560,280 -> 618,9
  (road city-loc-10 city-loc-6)
  (= (road-length city-loc-10 city-loc-6) 28)
  (= (fuel-demand city-loc-10 city-loc-6) 56)
  ; 618,9 -> 560,280
  (road city-loc-6 city-loc-10)
  (= (road-length city-loc-6 city-loc-10) 28)
  (= (fuel-demand city-loc-6 city-loc-10) 56)
  ; 458,249 -> 256,440
  (road city-loc-11 city-loc-3)
  (= (road-length city-loc-11 city-loc-3) 28)
  (= (fuel-demand city-loc-11 city-loc-3) 56)
  ; 256,440 -> 458,249
  (road city-loc-3 city-loc-11)
  (= (road-length city-loc-3 city-loc-11) 28)
  (= (fuel-demand city-loc-3 city-loc-11) 56)
  ; 458,249 -> 421,387
  (road city-loc-11 city-loc-5)
  (= (road-length city-loc-11 city-loc-5) 15)
  (= (fuel-demand city-loc-11 city-loc-5) 29)
  ; 421,387 -> 458,249
  (road city-loc-5 city-loc-11)
  (= (road-length city-loc-5 city-loc-11) 15)
  (= (fuel-demand city-loc-5 city-loc-11) 29)
  ; 458,249 -> 618,9
  (road city-loc-11 city-loc-6)
  (= (road-length city-loc-11 city-loc-6) 29)
  (= (fuel-demand city-loc-11 city-loc-6) 58)
  ; 618,9 -> 458,249
  (road city-loc-6 city-loc-11)
  (= (road-length city-loc-6 city-loc-11) 29)
  (= (fuel-demand city-loc-6 city-loc-11) 58)
  ; 458,249 -> 207,43
  (road city-loc-11 city-loc-7)
  (= (road-length city-loc-11 city-loc-7) 33)
  (= (fuel-demand city-loc-11 city-loc-7) 65)
  ; 207,43 -> 458,249
  (road city-loc-7 city-loc-11)
  (= (road-length city-loc-7 city-loc-11) 33)
  (= (fuel-demand city-loc-7 city-loc-11) 65)
  ; 458,249 -> 560,280
  (road city-loc-11 city-loc-10)
  (= (road-length city-loc-11 city-loc-10) 11)
  (= (fuel-demand city-loc-11 city-loc-10) 22)
  ; 560,280 -> 458,249
  (road city-loc-10 city-loc-11)
  (= (road-length city-loc-10 city-loc-11) 11)
  (= (fuel-demand city-loc-10 city-loc-11) 22)
  ; 444,484 -> 256,440
  (road city-loc-12 city-loc-3)
  (= (road-length city-loc-12 city-loc-3) 20)
  (= (fuel-demand city-loc-12 city-loc-3) 39)
  ; 256,440 -> 444,484
  (road city-loc-3 city-loc-12)
  (= (road-length city-loc-3 city-loc-12) 20)
  (= (fuel-demand city-loc-3 city-loc-12) 39)
  ; 444,484 -> 536,738
  (road city-loc-12 city-loc-4)
  (= (road-length city-loc-12 city-loc-4) 27)
  (= (fuel-demand city-loc-12 city-loc-4) 54)
  ; 536,738 -> 444,484
  (road city-loc-4 city-loc-12)
  (= (road-length city-loc-4 city-loc-12) 27)
  (= (fuel-demand city-loc-4 city-loc-12) 54)
  ; 444,484 -> 421,387
  (road city-loc-12 city-loc-5)
  (= (road-length city-loc-12 city-loc-5) 10)
  (= (fuel-demand city-loc-12 city-loc-5) 20)
  ; 421,387 -> 444,484
  (road city-loc-5 city-loc-12)
  (= (road-length city-loc-5 city-loc-12) 10)
  (= (fuel-demand city-loc-5 city-loc-12) 20)
  ; 444,484 -> 374,712
  (road city-loc-12 city-loc-8)
  (= (road-length city-loc-12 city-loc-8) 24)
  (= (fuel-demand city-loc-12 city-loc-8) 48)
  ; 374,712 -> 444,484
  (road city-loc-8 city-loc-12)
  (= (road-length city-loc-8 city-loc-12) 24)
  (= (fuel-demand city-loc-8 city-loc-12) 48)
  ; 444,484 -> 560,280
  (road city-loc-12 city-loc-10)
  (= (road-length city-loc-12 city-loc-10) 24)
  (= (fuel-demand city-loc-12 city-loc-10) 47)
  ; 560,280 -> 444,484
  (road city-loc-10 city-loc-12)
  (= (road-length city-loc-10 city-loc-12) 24)
  (= (fuel-demand city-loc-10 city-loc-12) 47)
  ; 444,484 -> 458,249
  (road city-loc-12 city-loc-11)
  (= (road-length city-loc-12 city-loc-11) 24)
  (= (fuel-demand city-loc-12 city-loc-11) 47)
  ; 458,249 -> 444,484
  (road city-loc-11 city-loc-12)
  (= (road-length city-loc-11 city-loc-12) 24)
  (= (fuel-demand city-loc-11 city-loc-12) 47)
  ; 147,230 -> 256,440
  (road city-loc-13 city-loc-3)
  (= (road-length city-loc-13 city-loc-3) 24)
  (= (fuel-demand city-loc-13 city-loc-3) 48)
  ; 256,440 -> 147,230
  (road city-loc-3 city-loc-13)
  (= (road-length city-loc-3 city-loc-13) 24)
  (= (fuel-demand city-loc-3 city-loc-13) 48)
  ; 147,230 -> 421,387
  (road city-loc-13 city-loc-5)
  (= (road-length city-loc-13 city-loc-5) 32)
  (= (fuel-demand city-loc-13 city-loc-5) 64)
  ; 421,387 -> 147,230
  (road city-loc-5 city-loc-13)
  (= (road-length city-loc-5 city-loc-13) 32)
  (= (fuel-demand city-loc-5 city-loc-13) 64)
  ; 147,230 -> 207,43
  (road city-loc-13 city-loc-7)
  (= (road-length city-loc-13 city-loc-7) 20)
  (= (fuel-demand city-loc-13 city-loc-7) 40)
  ; 207,43 -> 147,230
  (road city-loc-7 city-loc-13)
  (= (road-length city-loc-7 city-loc-13) 20)
  (= (fuel-demand city-loc-7 city-loc-13) 40)
  ; 147,230 -> 458,249
  (road city-loc-13 city-loc-11)
  (= (road-length city-loc-13 city-loc-11) 32)
  (= (fuel-demand city-loc-13 city-loc-11) 63)
  ; 458,249 -> 147,230
  (road city-loc-11 city-loc-13)
  (= (road-length city-loc-11 city-loc-13) 32)
  (= (fuel-demand city-loc-11 city-loc-13) 63)
  (at package-1 city-loc-5)
  (at package-2 city-loc-10)
  (at package-3 city-loc-8)
  (at package-4 city-loc-7)
  (at package-5 city-loc-3)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-1)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 409)
  (= (fuel-max truck-1) 409)
  (at truck-2 city-loc-1)
  (capacity truck-2 capacity-2)
  (= (fuel-left truck-2) 409)
  (= (fuel-max truck-2) 409)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-2))
  (preference delivery-2 (at package-2 city-loc-5))
  (preference delivery-3 (at package-3 city-loc-9))
  (preference delivery-4 (at package-4 city-loc-5))
  (preference delivery-5 (at package-5 city-loc-5))
 ))
 (:metric maximize
   (- 465
     (+ (total-cost)
       (* (is-violated delivery-1) 78)
       (* (is-violated delivery-2) 54)
       (* (is-violated delivery-3) 109)
       (* (is-violated delivery-4) 118)
       (* (is-violated delivery-5) 106)
     )
   )
 )
)
