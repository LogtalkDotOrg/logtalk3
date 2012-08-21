; Transport city-netbenefit-1petrol-station-15nodes-1000size-3degree-100mindistance-2trucks-6packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-15nodes-1000size-3degree-100mindistance-2trucks-6packagespercity-4016seed)
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
  city-loc-15 - location
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
  ; 374,712 -> 536,738
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 17)
  (= (fuel-demand city-loc-5 city-loc-1) 33)
  ; 536,738 -> 374,712
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 17)
  (= (fuel-demand city-loc-1 city-loc-5) 33)
  ; 436,887 -> 536,738
  (road city-loc-6 city-loc-1)
  (= (road-length city-loc-6 city-loc-1) 18)
  (= (fuel-demand city-loc-6 city-loc-1) 36)
  ; 536,738 -> 436,887
  (road city-loc-1 city-loc-6)
  (= (road-length city-loc-1 city-loc-6) 18)
  (= (fuel-demand city-loc-1 city-loc-6) 36)
  ; 436,887 -> 374,712
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 19)
  (= (fuel-demand city-loc-6 city-loc-5) 38)
  ; 374,712 -> 436,887
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 19)
  (= (fuel-demand city-loc-5 city-loc-6) 38)
  ; 825,140 -> 618,9
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 25)
  (= (fuel-demand city-loc-7 city-loc-3) 49)
  ; 618,9 -> 825,140
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 25)
  (= (fuel-demand city-loc-3 city-loc-7) 49)
  ; 560,280 -> 421,387
  (road city-loc-8 city-loc-2)
  (= (road-length city-loc-8 city-loc-2) 18)
  (= (fuel-demand city-loc-8 city-loc-2) 35)
  ; 421,387 -> 560,280
  (road city-loc-2 city-loc-8)
  (= (road-length city-loc-2 city-loc-8) 18)
  (= (fuel-demand city-loc-2 city-loc-8) 35)
  ; 560,280 -> 618,9
  (road city-loc-8 city-loc-3)
  (= (road-length city-loc-8 city-loc-3) 28)
  (= (fuel-demand city-loc-8 city-loc-3) 56)
  ; 618,9 -> 560,280
  (road city-loc-3 city-loc-8)
  (= (road-length city-loc-3 city-loc-8) 28)
  (= (fuel-demand city-loc-3 city-loc-8) 56)
  ; 560,280 -> 825,140
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 30)
  (= (fuel-demand city-loc-8 city-loc-7) 60)
  ; 825,140 -> 560,280
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 30)
  (= (fuel-demand city-loc-7 city-loc-8) 60)
  ; 458,249 -> 421,387
  (road city-loc-9 city-loc-2)
  (= (road-length city-loc-9 city-loc-2) 15)
  (= (fuel-demand city-loc-9 city-loc-2) 29)
  ; 421,387 -> 458,249
  (road city-loc-2 city-loc-9)
  (= (road-length city-loc-2 city-loc-9) 15)
  (= (fuel-demand city-loc-2 city-loc-9) 29)
  ; 458,249 -> 618,9
  (road city-loc-9 city-loc-3)
  (= (road-length city-loc-9 city-loc-3) 29)
  (= (fuel-demand city-loc-9 city-loc-3) 58)
  ; 618,9 -> 458,249
  (road city-loc-3 city-loc-9)
  (= (road-length city-loc-3 city-loc-9) 29)
  (= (fuel-demand city-loc-3 city-loc-9) 58)
  ; 458,249 -> 560,280
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 11)
  (= (fuel-demand city-loc-9 city-loc-8) 22)
  ; 560,280 -> 458,249
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 11)
  (= (fuel-demand city-loc-8 city-loc-9) 22)
  ; 444,484 -> 536,738
  (road city-loc-10 city-loc-1)
  (= (road-length city-loc-10 city-loc-1) 27)
  (= (fuel-demand city-loc-10 city-loc-1) 54)
  ; 536,738 -> 444,484
  (road city-loc-1 city-loc-10)
  (= (road-length city-loc-1 city-loc-10) 27)
  (= (fuel-demand city-loc-1 city-loc-10) 54)
  ; 444,484 -> 421,387
  (road city-loc-10 city-loc-2)
  (= (road-length city-loc-10 city-loc-2) 10)
  (= (fuel-demand city-loc-10 city-loc-2) 20)
  ; 421,387 -> 444,484
  (road city-loc-2 city-loc-10)
  (= (road-length city-loc-2 city-loc-10) 10)
  (= (fuel-demand city-loc-2 city-loc-10) 20)
  ; 444,484 -> 374,712
  (road city-loc-10 city-loc-5)
  (= (road-length city-loc-10 city-loc-5) 24)
  (= (fuel-demand city-loc-10 city-loc-5) 48)
  ; 374,712 -> 444,484
  (road city-loc-5 city-loc-10)
  (= (road-length city-loc-5 city-loc-10) 24)
  (= (fuel-demand city-loc-5 city-loc-10) 48)
  ; 444,484 -> 560,280
  (road city-loc-10 city-loc-8)
  (= (road-length city-loc-10 city-loc-8) 24)
  (= (fuel-demand city-loc-10 city-loc-8) 47)
  ; 560,280 -> 444,484
  (road city-loc-8 city-loc-10)
  (= (road-length city-loc-8 city-loc-10) 24)
  (= (fuel-demand city-loc-8 city-loc-10) 47)
  ; 444,484 -> 458,249
  (road city-loc-10 city-loc-9)
  (= (road-length city-loc-10 city-loc-9) 24)
  (= (fuel-demand city-loc-10 city-loc-9) 47)
  ; 458,249 -> 444,484
  (road city-loc-9 city-loc-10)
  (= (road-length city-loc-9 city-loc-10) 24)
  (= (fuel-demand city-loc-9 city-loc-10) 47)
  ; 147,230 -> 207,43
  (road city-loc-11 city-loc-4)
  (= (road-length city-loc-11 city-loc-4) 20)
  (= (fuel-demand city-loc-11 city-loc-4) 40)
  ; 207,43 -> 147,230
  (road city-loc-4 city-loc-11)
  (= (road-length city-loc-4 city-loc-11) 20)
  (= (fuel-demand city-loc-4 city-loc-11) 40)
  ; 595,480 -> 536,738
  (road city-loc-12 city-loc-1)
  (= (road-length city-loc-12 city-loc-1) 27)
  (= (fuel-demand city-loc-12 city-loc-1) 53)
  ; 536,738 -> 595,480
  (road city-loc-1 city-loc-12)
  (= (road-length city-loc-1 city-loc-12) 27)
  (= (fuel-demand city-loc-1 city-loc-12) 53)
  ; 595,480 -> 421,387
  (road city-loc-12 city-loc-2)
  (= (road-length city-loc-12 city-loc-2) 20)
  (= (fuel-demand city-loc-12 city-loc-2) 40)
  ; 421,387 -> 595,480
  (road city-loc-2 city-loc-12)
  (= (road-length city-loc-2 city-loc-12) 20)
  (= (fuel-demand city-loc-2 city-loc-12) 40)
  ; 595,480 -> 560,280
  (road city-loc-12 city-loc-8)
  (= (road-length city-loc-12 city-loc-8) 21)
  (= (fuel-demand city-loc-12 city-loc-8) 41)
  ; 560,280 -> 595,480
  (road city-loc-8 city-loc-12)
  (= (road-length city-loc-8 city-loc-12) 21)
  (= (fuel-demand city-loc-8 city-loc-12) 41)
  ; 595,480 -> 458,249
  (road city-loc-12 city-loc-9)
  (= (road-length city-loc-12 city-loc-9) 27)
  (= (fuel-demand city-loc-12 city-loc-9) 54)
  ; 458,249 -> 595,480
  (road city-loc-9 city-loc-12)
  (= (road-length city-loc-9 city-loc-12) 27)
  (= (fuel-demand city-loc-9 city-loc-12) 54)
  ; 595,480 -> 444,484
  (road city-loc-12 city-loc-10)
  (= (road-length city-loc-12 city-loc-10) 16)
  (= (fuel-demand city-loc-12 city-loc-10) 31)
  ; 444,484 -> 595,480
  (road city-loc-10 city-loc-12)
  (= (road-length city-loc-10 city-loc-12) 16)
  (= (fuel-demand city-loc-10 city-loc-12) 31)
  ; 226,396 -> 421,387
  (road city-loc-13 city-loc-2)
  (= (road-length city-loc-13 city-loc-2) 20)
  (= (fuel-demand city-loc-13 city-loc-2) 39)
  ; 421,387 -> 226,396
  (road city-loc-2 city-loc-13)
  (= (road-length city-loc-2 city-loc-13) 20)
  (= (fuel-demand city-loc-2 city-loc-13) 39)
  ; 226,396 -> 458,249
  (road city-loc-13 city-loc-9)
  (= (road-length city-loc-13 city-loc-9) 28)
  (= (fuel-demand city-loc-13 city-loc-9) 55)
  ; 458,249 -> 226,396
  (road city-loc-9 city-loc-13)
  (= (road-length city-loc-9 city-loc-13) 28)
  (= (fuel-demand city-loc-9 city-loc-13) 55)
  ; 226,396 -> 444,484
  (road city-loc-13 city-loc-10)
  (= (road-length city-loc-13 city-loc-10) 24)
  (= (fuel-demand city-loc-13 city-loc-10) 47)
  ; 444,484 -> 226,396
  (road city-loc-10 city-loc-13)
  (= (road-length city-loc-10 city-loc-13) 24)
  (= (fuel-demand city-loc-10 city-loc-13) 47)
  ; 226,396 -> 147,230
  (road city-loc-13 city-loc-11)
  (= (road-length city-loc-13 city-loc-11) 19)
  (= (fuel-demand city-loc-13 city-loc-11) 37)
  ; 147,230 -> 226,396
  (road city-loc-11 city-loc-13)
  (= (road-length city-loc-11 city-loc-13) 19)
  (= (fuel-demand city-loc-11 city-loc-13) 37)
  ; 49,283 -> 207,43
  (road city-loc-14 city-loc-4)
  (= (road-length city-loc-14 city-loc-4) 29)
  (= (fuel-demand city-loc-14 city-loc-4) 58)
  ; 207,43 -> 49,283
  (road city-loc-4 city-loc-14)
  (= (road-length city-loc-4 city-loc-14) 29)
  (= (fuel-demand city-loc-4 city-loc-14) 58)
  ; 49,283 -> 147,230
  (road city-loc-14 city-loc-11)
  (= (road-length city-loc-14 city-loc-11) 12)
  (= (fuel-demand city-loc-14 city-loc-11) 23)
  ; 147,230 -> 49,283
  (road city-loc-11 city-loc-14)
  (= (road-length city-loc-11 city-loc-14) 12)
  (= (fuel-demand city-loc-11 city-loc-14) 23)
  ; 49,283 -> 226,396
  (road city-loc-14 city-loc-13)
  (= (road-length city-loc-14 city-loc-13) 21)
  (= (fuel-demand city-loc-14 city-loc-13) 42)
  ; 226,396 -> 49,283
  (road city-loc-13 city-loc-14)
  (= (road-length city-loc-13 city-loc-14) 21)
  (= (fuel-demand city-loc-13 city-loc-14) 42)
  ; 14,77 -> 207,43
  (road city-loc-15 city-loc-4)
  (= (road-length city-loc-15 city-loc-4) 20)
  (= (fuel-demand city-loc-15 city-loc-4) 40)
  ; 207,43 -> 14,77
  (road city-loc-4 city-loc-15)
  (= (road-length city-loc-4 city-loc-15) 20)
  (= (fuel-demand city-loc-4 city-loc-15) 40)
  ; 14,77 -> 147,230
  (road city-loc-15 city-loc-11)
  (= (road-length city-loc-15 city-loc-11) 21)
  (= (fuel-demand city-loc-15 city-loc-11) 41)
  ; 147,230 -> 14,77
  (road city-loc-11 city-loc-15)
  (= (road-length city-loc-11 city-loc-15) 21)
  (= (fuel-demand city-loc-11 city-loc-15) 41)
  ; 14,77 -> 49,283
  (road city-loc-15 city-loc-14)
  (= (road-length city-loc-15 city-loc-14) 21)
  (= (fuel-demand city-loc-15 city-loc-14) 42)
  ; 49,283 -> 14,77
  (road city-loc-14 city-loc-15)
  (= (road-length city-loc-14 city-loc-15) 21)
  (= (fuel-demand city-loc-14 city-loc-15) 42)
  (at package-1 city-loc-6)
  (at package-2 city-loc-9)
  (at package-3 city-loc-10)
  (at package-4 city-loc-6)
  (at package-5 city-loc-5)
  (at package-6 city-loc-5)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-9)
  (capacity truck-1 capacity-2)
  (= (fuel-left truck-1) 392)
  (= (fuel-max truck-1) 392)
  (at truck-2 city-loc-9)
  (capacity truck-2 capacity-4)
  (= (fuel-left truck-2) 392)
  (= (fuel-max truck-2) 392)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-7))
  (preference delivery-2 (at package-2 city-loc-15))
  (preference delivery-3 (at package-3 city-loc-9))
  (preference delivery-4 (at package-4 city-loc-2))
  (preference delivery-5 (at package-5 city-loc-4))
  (preference delivery-6 (at package-6 city-loc-10))
 ))
 (:metric maximize
   (- 620
     (+ (total-cost)
       (* (is-violated delivery-1) 92)
       (* (is-violated delivery-2) 125)
       (* (is-violated delivery-3) 131)
       (* (is-violated delivery-4) 68)
       (* (is-violated delivery-5) 81)
       (* (is-violated delivery-6) 123)
     )
   )
 )
)
