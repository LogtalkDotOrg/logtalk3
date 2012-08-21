; Transport city-netbenefit-1petrol-station-14nodes-1000size-3degree-100mindistance-2trucks-6packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-14nodes-1000size-3degree-100mindistance-2trucks-6packagespercity-4016seed)
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
  city-loc-14 - location
  truck-1 - vehicle
  truck-2 - vehicle
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
  ; 256,440 -> 53,545
  (road city-loc-2 city-loc-1)
  (= (road-length city-loc-2 city-loc-1) 23)
  (= (fuel-demand city-loc-2 city-loc-1) 46)
  ; 53,545 -> 256,440
  (road city-loc-1 city-loc-2)
  (= (road-length city-loc-1 city-loc-2) 23)
  (= (fuel-demand city-loc-1 city-loc-2) 46)
  ; 421,387 -> 256,440
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 18)
  (= (fuel-demand city-loc-4 city-loc-2) 35)
  ; 256,440 -> 421,387
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 18)
  (= (fuel-demand city-loc-2 city-loc-4) 35)
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
  ; 825,140 -> 618,9
  (road city-loc-9 city-loc-5)
  (= (road-length city-loc-9 city-loc-5) 25)
  (= (fuel-demand city-loc-9 city-loc-5) 49)
  ; 618,9 -> 825,140
  (road city-loc-5 city-loc-9)
  (= (road-length city-loc-5 city-loc-9) 25)
  (= (fuel-demand city-loc-5 city-loc-9) 49)
  ; 560,280 -> 421,387
  (road city-loc-10 city-loc-4)
  (= (road-length city-loc-10 city-loc-4) 18)
  (= (fuel-demand city-loc-10 city-loc-4) 35)
  ; 421,387 -> 560,280
  (road city-loc-4 city-loc-10)
  (= (road-length city-loc-4 city-loc-10) 18)
  (= (fuel-demand city-loc-4 city-loc-10) 35)
  ; 560,280 -> 618,9
  (road city-loc-10 city-loc-5)
  (= (road-length city-loc-10 city-loc-5) 28)
  (= (fuel-demand city-loc-10 city-loc-5) 56)
  ; 618,9 -> 560,280
  (road city-loc-5 city-loc-10)
  (= (road-length city-loc-5 city-loc-10) 28)
  (= (fuel-demand city-loc-5 city-loc-10) 56)
  ; 560,280 -> 825,140
  (road city-loc-10 city-loc-9)
  (= (road-length city-loc-10 city-loc-9) 30)
  (= (fuel-demand city-loc-10 city-loc-9) 60)
  ; 825,140 -> 560,280
  (road city-loc-9 city-loc-10)
  (= (road-length city-loc-9 city-loc-10) 30)
  (= (fuel-demand city-loc-9 city-loc-10) 60)
  ; 458,249 -> 256,440
  (road city-loc-11 city-loc-2)
  (= (road-length city-loc-11 city-loc-2) 28)
  (= (fuel-demand city-loc-11 city-loc-2) 56)
  ; 256,440 -> 458,249
  (road city-loc-2 city-loc-11)
  (= (road-length city-loc-2 city-loc-11) 28)
  (= (fuel-demand city-loc-2 city-loc-11) 56)
  ; 458,249 -> 421,387
  (road city-loc-11 city-loc-4)
  (= (road-length city-loc-11 city-loc-4) 15)
  (= (fuel-demand city-loc-11 city-loc-4) 29)
  ; 421,387 -> 458,249
  (road city-loc-4 city-loc-11)
  (= (road-length city-loc-4 city-loc-11) 15)
  (= (fuel-demand city-loc-4 city-loc-11) 29)
  ; 458,249 -> 618,9
  (road city-loc-11 city-loc-5)
  (= (road-length city-loc-11 city-loc-5) 29)
  (= (fuel-demand city-loc-11 city-loc-5) 58)
  ; 618,9 -> 458,249
  (road city-loc-5 city-loc-11)
  (= (road-length city-loc-5 city-loc-11) 29)
  (= (fuel-demand city-loc-5 city-loc-11) 58)
  ; 458,249 -> 560,280
  (road city-loc-11 city-loc-10)
  (= (road-length city-loc-11 city-loc-10) 11)
  (= (fuel-demand city-loc-11 city-loc-10) 22)
  ; 560,280 -> 458,249
  (road city-loc-10 city-loc-11)
  (= (road-length city-loc-10 city-loc-11) 11)
  (= (fuel-demand city-loc-10 city-loc-11) 22)
  ; 444,484 -> 256,440
  (road city-loc-12 city-loc-2)
  (= (road-length city-loc-12 city-loc-2) 20)
  (= (fuel-demand city-loc-12 city-loc-2) 39)
  ; 256,440 -> 444,484
  (road city-loc-2 city-loc-12)
  (= (road-length city-loc-2 city-loc-12) 20)
  (= (fuel-demand city-loc-2 city-loc-12) 39)
  ; 444,484 -> 536,738
  (road city-loc-12 city-loc-3)
  (= (road-length city-loc-12 city-loc-3) 27)
  (= (fuel-demand city-loc-12 city-loc-3) 54)
  ; 536,738 -> 444,484
  (road city-loc-3 city-loc-12)
  (= (road-length city-loc-3 city-loc-12) 27)
  (= (fuel-demand city-loc-3 city-loc-12) 54)
  ; 444,484 -> 421,387
  (road city-loc-12 city-loc-4)
  (= (road-length city-loc-12 city-loc-4) 10)
  (= (fuel-demand city-loc-12 city-loc-4) 20)
  ; 421,387 -> 444,484
  (road city-loc-4 city-loc-12)
  (= (road-length city-loc-4 city-loc-12) 10)
  (= (fuel-demand city-loc-4 city-loc-12) 20)
  ; 444,484 -> 374,712
  (road city-loc-12 city-loc-7)
  (= (road-length city-loc-12 city-loc-7) 24)
  (= (fuel-demand city-loc-12 city-loc-7) 48)
  ; 374,712 -> 444,484
  (road city-loc-7 city-loc-12)
  (= (road-length city-loc-7 city-loc-12) 24)
  (= (fuel-demand city-loc-7 city-loc-12) 48)
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
  (road city-loc-13 city-loc-2)
  (= (road-length city-loc-13 city-loc-2) 24)
  (= (fuel-demand city-loc-13 city-loc-2) 48)
  ; 256,440 -> 147,230
  (road city-loc-2 city-loc-13)
  (= (road-length city-loc-2 city-loc-13) 24)
  (= (fuel-demand city-loc-2 city-loc-13) 48)
  ; 147,230 -> 207,43
  (road city-loc-13 city-loc-6)
  (= (road-length city-loc-13 city-loc-6) 20)
  (= (fuel-demand city-loc-13 city-loc-6) 40)
  ; 207,43 -> 147,230
  (road city-loc-6 city-loc-13)
  (= (road-length city-loc-6 city-loc-13) 20)
  (= (fuel-demand city-loc-6 city-loc-13) 40)
  ; 147,230 -> 458,249
  (road city-loc-13 city-loc-11)
  (= (road-length city-loc-13 city-loc-11) 32)
  (= (fuel-demand city-loc-13 city-loc-11) 63)
  ; 458,249 -> 147,230
  (road city-loc-11 city-loc-13)
  (= (road-length city-loc-11 city-loc-13) 32)
  (= (fuel-demand city-loc-11 city-loc-13) 63)
  ; 595,480 -> 536,738
  (road city-loc-14 city-loc-3)
  (= (road-length city-loc-14 city-loc-3) 27)
  (= (fuel-demand city-loc-14 city-loc-3) 53)
  ; 536,738 -> 595,480
  (road city-loc-3 city-loc-14)
  (= (road-length city-loc-3 city-loc-14) 27)
  (= (fuel-demand city-loc-3 city-loc-14) 53)
  ; 595,480 -> 421,387
  (road city-loc-14 city-loc-4)
  (= (road-length city-loc-14 city-loc-4) 20)
  (= (fuel-demand city-loc-14 city-loc-4) 40)
  ; 421,387 -> 595,480
  (road city-loc-4 city-loc-14)
  (= (road-length city-loc-4 city-loc-14) 20)
  (= (fuel-demand city-loc-4 city-loc-14) 40)
  ; 595,480 -> 560,280
  (road city-loc-14 city-loc-10)
  (= (road-length city-loc-14 city-loc-10) 21)
  (= (fuel-demand city-loc-14 city-loc-10) 41)
  ; 560,280 -> 595,480
  (road city-loc-10 city-loc-14)
  (= (road-length city-loc-10 city-loc-14) 21)
  (= (fuel-demand city-loc-10 city-loc-14) 41)
  ; 595,480 -> 458,249
  (road city-loc-14 city-loc-11)
  (= (road-length city-loc-14 city-loc-11) 27)
  (= (fuel-demand city-loc-14 city-loc-11) 54)
  ; 458,249 -> 595,480
  (road city-loc-11 city-loc-14)
  (= (road-length city-loc-11 city-loc-14) 27)
  (= (fuel-demand city-loc-11 city-loc-14) 54)
  ; 595,480 -> 444,484
  (road city-loc-14 city-loc-12)
  (= (road-length city-loc-14 city-loc-12) 16)
  (= (fuel-demand city-loc-14 city-loc-12) 31)
  ; 444,484 -> 595,480
  (road city-loc-12 city-loc-14)
  (= (road-length city-loc-12 city-loc-14) 16)
  (= (fuel-demand city-loc-12 city-loc-14) 31)
  (at package-1 city-loc-4)
  (at package-2 city-loc-6)
  (at package-3 city-loc-1)
  (at package-4 city-loc-4)
  (at package-5 city-loc-1)
  (at package-6 city-loc-2)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-9)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 386)
  (= (fuel-max truck-1) 386)
  (at truck-2 city-loc-6)
  (capacity truck-2 capacity-3)
  (= (fuel-left truck-2) 386)
  (= (fuel-max truck-2) 386)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-5))
  (preference delivery-2 (at package-2 city-loc-4))
  (preference delivery-3 (at package-3 city-loc-9))
  (preference delivery-4 (at package-4 city-loc-10))
  (preference delivery-5 (at package-5 city-loc-8))
  (preference delivery-6 (at package-6 city-loc-6))
 ))
 (:metric maximize
   (- 581
     (+ (total-cost)
       (* (is-violated delivery-1) 147)
       (* (is-violated delivery-2) 105)
       (* (is-violated delivery-3) 58)
       (* (is-violated delivery-4) 74)
       (* (is-violated delivery-5) 83)
       (* (is-violated delivery-6) 114)
     )
   )
 )
)
