; Transport city-netbenefit-1petrol-station-10nodes-1000size-3degree-100mindistance-2trucks-4packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-10nodes-1000size-3degree-100mindistance-2trucks-4packagespercity-4016seed)
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
  truck-1 - vehicle
  truck-2 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
  package-4 - package
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
  ; 436,887 -> 374,712
  (road city-loc-3 city-loc-2)
  (= (road-length city-loc-3 city-loc-2) 19)
  (= (fuel-demand city-loc-3 city-loc-2) 38)
  ; 374,712 -> 436,887
  (road city-loc-2 city-loc-3)
  (= (road-length city-loc-2 city-loc-3) 19)
  (= (fuel-demand city-loc-2 city-loc-3) 38)
  ; 350,353 -> 207,43
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 35)
  (= (fuel-demand city-loc-5 city-loc-1) 69)
  ; 207,43 -> 350,353
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 35)
  (= (fuel-demand city-loc-1 city-loc-5) 69)
  ; 350,353 -> 374,712
  (road city-loc-5 city-loc-2)
  (= (road-length city-loc-5 city-loc-2) 36)
  (= (fuel-demand city-loc-5 city-loc-2) 72)
  ; 374,712 -> 350,353
  (road city-loc-2 city-loc-5)
  (= (road-length city-loc-2 city-loc-5) 36)
  (= (fuel-demand city-loc-2 city-loc-5) 72)
  ; 560,280 -> 825,140
  (road city-loc-6 city-loc-4)
  (= (road-length city-loc-6 city-loc-4) 30)
  (= (fuel-demand city-loc-6 city-loc-4) 60)
  ; 825,140 -> 560,280
  (road city-loc-4 city-loc-6)
  (= (road-length city-loc-4 city-loc-6) 30)
  (= (fuel-demand city-loc-4 city-loc-6) 60)
  ; 560,280 -> 350,353
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 23)
  (= (fuel-demand city-loc-6 city-loc-5) 45)
  ; 350,353 -> 560,280
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 23)
  (= (fuel-demand city-loc-5 city-loc-6) 45)
  ; 458,249 -> 207,43
  (road city-loc-7 city-loc-1)
  (= (road-length city-loc-7 city-loc-1) 33)
  (= (fuel-demand city-loc-7 city-loc-1) 65)
  ; 207,43 -> 458,249
  (road city-loc-1 city-loc-7)
  (= (road-length city-loc-1 city-loc-7) 33)
  (= (fuel-demand city-loc-1 city-loc-7) 65)
  ; 458,249 -> 350,353
  (road city-loc-7 city-loc-5)
  (= (road-length city-loc-7 city-loc-5) 15)
  (= (fuel-demand city-loc-7 city-loc-5) 30)
  ; 350,353 -> 458,249
  (road city-loc-5 city-loc-7)
  (= (road-length city-loc-5 city-loc-7) 15)
  (= (fuel-demand city-loc-5 city-loc-7) 30)
  ; 458,249 -> 560,280
  (road city-loc-7 city-loc-6)
  (= (road-length city-loc-7 city-loc-6) 11)
  (= (fuel-demand city-loc-7 city-loc-6) 22)
  ; 560,280 -> 458,249
  (road city-loc-6 city-loc-7)
  (= (road-length city-loc-6 city-loc-7) 11)
  (= (fuel-demand city-loc-6 city-loc-7) 22)
  ; 444,484 -> 374,712
  (road city-loc-8 city-loc-2)
  (= (road-length city-loc-8 city-loc-2) 24)
  (= (fuel-demand city-loc-8 city-loc-2) 48)
  ; 374,712 -> 444,484
  (road city-loc-2 city-loc-8)
  (= (road-length city-loc-2 city-loc-8) 24)
  (= (fuel-demand city-loc-2 city-loc-8) 48)
  ; 444,484 -> 350,353
  (road city-loc-8 city-loc-5)
  (= (road-length city-loc-8 city-loc-5) 17)
  (= (fuel-demand city-loc-8 city-loc-5) 33)
  ; 350,353 -> 444,484
  (road city-loc-5 city-loc-8)
  (= (road-length city-loc-5 city-loc-8) 17)
  (= (fuel-demand city-loc-5 city-loc-8) 33)
  ; 444,484 -> 560,280
  (road city-loc-8 city-loc-6)
  (= (road-length city-loc-8 city-loc-6) 24)
  (= (fuel-demand city-loc-8 city-loc-6) 47)
  ; 560,280 -> 444,484
  (road city-loc-6 city-loc-8)
  (= (road-length city-loc-6 city-loc-8) 24)
  (= (fuel-demand city-loc-6 city-loc-8) 47)
  ; 444,484 -> 458,249
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 24)
  (= (fuel-demand city-loc-8 city-loc-7) 47)
  ; 458,249 -> 444,484
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 24)
  (= (fuel-demand city-loc-7 city-loc-8) 47)
  ; 147,230 -> 207,43
  (road city-loc-9 city-loc-1)
  (= (road-length city-loc-9 city-loc-1) 20)
  (= (fuel-demand city-loc-9 city-loc-1) 40)
  ; 207,43 -> 147,230
  (road city-loc-1 city-loc-9)
  (= (road-length city-loc-1 city-loc-9) 20)
  (= (fuel-demand city-loc-1 city-loc-9) 40)
  ; 147,230 -> 350,353
  (road city-loc-9 city-loc-5)
  (= (road-length city-loc-9 city-loc-5) 24)
  (= (fuel-demand city-loc-9 city-loc-5) 48)
  ; 350,353 -> 147,230
  (road city-loc-5 city-loc-9)
  (= (road-length city-loc-5 city-loc-9) 24)
  (= (fuel-demand city-loc-5 city-loc-9) 48)
  ; 147,230 -> 458,249
  (road city-loc-9 city-loc-7)
  (= (road-length city-loc-9 city-loc-7) 32)
  (= (fuel-demand city-loc-9 city-loc-7) 63)
  ; 458,249 -> 147,230
  (road city-loc-7 city-loc-9)
  (= (road-length city-loc-7 city-loc-9) 32)
  (= (fuel-demand city-loc-7 city-loc-9) 63)
  ; 595,480 -> 374,712
  (road city-loc-10 city-loc-2)
  (= (road-length city-loc-10 city-loc-2) 32)
  (= (fuel-demand city-loc-10 city-loc-2) 64)
  ; 374,712 -> 595,480
  (road city-loc-2 city-loc-10)
  (= (road-length city-loc-2 city-loc-10) 32)
  (= (fuel-demand city-loc-2 city-loc-10) 64)
  ; 595,480 -> 350,353
  (road city-loc-10 city-loc-5)
  (= (road-length city-loc-10 city-loc-5) 28)
  (= (fuel-demand city-loc-10 city-loc-5) 56)
  ; 350,353 -> 595,480
  (road city-loc-5 city-loc-10)
  (= (road-length city-loc-5 city-loc-10) 28)
  (= (fuel-demand city-loc-5 city-loc-10) 56)
  ; 595,480 -> 560,280
  (road city-loc-10 city-loc-6)
  (= (road-length city-loc-10 city-loc-6) 21)
  (= (fuel-demand city-loc-10 city-loc-6) 41)
  ; 560,280 -> 595,480
  (road city-loc-6 city-loc-10)
  (= (road-length city-loc-6 city-loc-10) 21)
  (= (fuel-demand city-loc-6 city-loc-10) 41)
  ; 595,480 -> 458,249
  (road city-loc-10 city-loc-7)
  (= (road-length city-loc-10 city-loc-7) 27)
  (= (fuel-demand city-loc-10 city-loc-7) 54)
  ; 458,249 -> 595,480
  (road city-loc-7 city-loc-10)
  (= (road-length city-loc-7 city-loc-10) 27)
  (= (fuel-demand city-loc-7 city-loc-10) 54)
  ; 595,480 -> 444,484
  (road city-loc-10 city-loc-8)
  (= (road-length city-loc-10 city-loc-8) 16)
  (= (fuel-demand city-loc-10 city-loc-8) 31)
  ; 444,484 -> 595,480
  (road city-loc-8 city-loc-10)
  (= (road-length city-loc-8 city-loc-10) 16)
  (= (fuel-demand city-loc-8 city-loc-10) 31)
  (at package-1 city-loc-3)
  (at package-2 city-loc-4)
  (at package-3 city-loc-1)
  (at package-4 city-loc-3)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-1)
  (capacity truck-1 capacity-2)
  (= (fuel-left truck-1) 390)
  (= (fuel-max truck-1) 390)
  (at truck-2 city-loc-6)
  (capacity truck-2 capacity-3)
  (= (fuel-left truck-2) 390)
  (= (fuel-max truck-2) 390)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-7))
  (preference delivery-2 (at package-2 city-loc-3))
  (preference delivery-3 (at package-3 city-loc-6))
  (preference delivery-4 (at package-4 city-loc-7))
 ))
 (:metric maximize
   (- 449
     (+ (total-cost)
       (* (is-violated delivery-1) 106)
       (* (is-violated delivery-2) 91)
       (* (is-violated delivery-3) 147)
       (* (is-violated delivery-4) 105)
     )
   )
 )
)
