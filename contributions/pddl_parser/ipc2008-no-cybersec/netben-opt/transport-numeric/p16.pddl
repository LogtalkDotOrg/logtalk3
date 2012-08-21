; Transport city-netbenefit-1petrol-station-11nodes-1000size-3degree-100mindistance-2trucks-4packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-11nodes-1000size-3degree-100mindistance-2trucks-4packagespercity-4016seed)
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
  ; 780,138 -> 458,249
  (road city-loc-3 city-loc-1)
  (= (road-length city-loc-3 city-loc-1) 35)
  (= (fuel-demand city-loc-3 city-loc-1) 69)
  ; 458,249 -> 780,138
  (road city-loc-1 city-loc-3)
  (= (road-length city-loc-1 city-loc-3) 35)
  (= (fuel-demand city-loc-1 city-loc-3) 69)
  ; 529,363 -> 458,249
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 14)
  (= (fuel-demand city-loc-4 city-loc-1) 27)
  ; 458,249 -> 529,363
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 14)
  (= (fuel-demand city-loc-1 city-loc-4) 27)
  ; 529,363 -> 780,138
  (road city-loc-4 city-loc-3)
  (= (road-length city-loc-4 city-loc-3) 34)
  (= (fuel-demand city-loc-4 city-loc-3) 68)
  ; 780,138 -> 529,363
  (road city-loc-3 city-loc-4)
  (= (road-length city-loc-3 city-loc-4) 34)
  (= (fuel-demand city-loc-3 city-loc-4) 68)
  ; 444,484 -> 458,249
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 24)
  (= (fuel-demand city-loc-5 city-loc-1) 47)
  ; 458,249 -> 444,484
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 24)
  (= (fuel-demand city-loc-1 city-loc-5) 47)
  ; 444,484 -> 410,835
  (road city-loc-5 city-loc-2)
  (= (road-length city-loc-5 city-loc-2) 36)
  (= (fuel-demand city-loc-5 city-loc-2) 71)
  ; 410,835 -> 444,484
  (road city-loc-2 city-loc-5)
  (= (road-length city-loc-2 city-loc-5) 36)
  (= (fuel-demand city-loc-2 city-loc-5) 71)
  ; 444,484 -> 529,363
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 15)
  (= (fuel-demand city-loc-5 city-loc-4) 30)
  ; 529,363 -> 444,484
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 15)
  (= (fuel-demand city-loc-4 city-loc-5) 30)
  ; 147,230 -> 458,249
  (road city-loc-6 city-loc-1)
  (= (road-length city-loc-6 city-loc-1) 32)
  (= (fuel-demand city-loc-6 city-loc-1) 63)
  ; 458,249 -> 147,230
  (road city-loc-1 city-loc-6)
  (= (road-length city-loc-1 city-loc-6) 32)
  (= (fuel-demand city-loc-1 city-loc-6) 63)
  ; 310,758 -> 410,835
  (road city-loc-7 city-loc-2)
  (= (road-length city-loc-7 city-loc-2) 13)
  (= (fuel-demand city-loc-7 city-loc-2) 26)
  ; 410,835 -> 310,758
  (road city-loc-2 city-loc-7)
  (= (road-length city-loc-2 city-loc-7) 13)
  (= (fuel-demand city-loc-2 city-loc-7) 26)
  ; 310,758 -> 444,484
  (road city-loc-7 city-loc-5)
  (= (road-length city-loc-7 city-loc-5) 31)
  (= (fuel-demand city-loc-7 city-loc-5) 61)
  ; 444,484 -> 310,758
  (road city-loc-5 city-loc-7)
  (= (road-length city-loc-5 city-loc-7) 31)
  (= (fuel-demand city-loc-5 city-loc-7) 61)
  ; 595,480 -> 458,249
  (road city-loc-8 city-loc-1)
  (= (road-length city-loc-8 city-loc-1) 27)
  (= (fuel-demand city-loc-8 city-loc-1) 54)
  ; 458,249 -> 595,480
  (road city-loc-1 city-loc-8)
  (= (road-length city-loc-1 city-loc-8) 27)
  (= (fuel-demand city-loc-1 city-loc-8) 54)
  ; 595,480 -> 529,363
  (road city-loc-8 city-loc-4)
  (= (road-length city-loc-8 city-loc-4) 14)
  (= (fuel-demand city-loc-8 city-loc-4) 27)
  ; 529,363 -> 595,480
  (road city-loc-4 city-loc-8)
  (= (road-length city-loc-4 city-loc-8) 14)
  (= (fuel-demand city-loc-4 city-loc-8) 27)
  ; 595,480 -> 444,484
  (road city-loc-8 city-loc-5)
  (= (road-length city-loc-8 city-loc-5) 16)
  (= (fuel-demand city-loc-8 city-loc-5) 31)
  ; 444,484 -> 595,480
  (road city-loc-5 city-loc-8)
  (= (road-length city-loc-5 city-loc-8) 16)
  (= (fuel-demand city-loc-5 city-loc-8) 31)
  ; 226,396 -> 458,249
  (road city-loc-9 city-loc-1)
  (= (road-length city-loc-9 city-loc-1) 28)
  (= (fuel-demand city-loc-9 city-loc-1) 55)
  ; 458,249 -> 226,396
  (road city-loc-1 city-loc-9)
  (= (road-length city-loc-1 city-loc-9) 28)
  (= (fuel-demand city-loc-1 city-loc-9) 55)
  ; 226,396 -> 529,363
  (road city-loc-9 city-loc-4)
  (= (road-length city-loc-9 city-loc-4) 31)
  (= (fuel-demand city-loc-9 city-loc-4) 61)
  ; 529,363 -> 226,396
  (road city-loc-4 city-loc-9)
  (= (road-length city-loc-4 city-loc-9) 31)
  (= (fuel-demand city-loc-4 city-loc-9) 61)
  ; 226,396 -> 444,484
  (road city-loc-9 city-loc-5)
  (= (road-length city-loc-9 city-loc-5) 24)
  (= (fuel-demand city-loc-9 city-loc-5) 47)
  ; 444,484 -> 226,396
  (road city-loc-5 city-loc-9)
  (= (road-length city-loc-5 city-loc-9) 24)
  (= (fuel-demand city-loc-5 city-loc-9) 47)
  ; 226,396 -> 147,230
  (road city-loc-9 city-loc-6)
  (= (road-length city-loc-9 city-loc-6) 19)
  (= (fuel-demand city-loc-9 city-loc-6) 37)
  ; 147,230 -> 226,396
  (road city-loc-6 city-loc-9)
  (= (road-length city-loc-6 city-loc-9) 19)
  (= (fuel-demand city-loc-6 city-loc-9) 37)
  ; 49,283 -> 147,230
  (road city-loc-10 city-loc-6)
  (= (road-length city-loc-10 city-loc-6) 12)
  (= (fuel-demand city-loc-10 city-loc-6) 23)
  ; 147,230 -> 49,283
  (road city-loc-6 city-loc-10)
  (= (road-length city-loc-6 city-loc-10) 12)
  (= (fuel-demand city-loc-6 city-loc-10) 23)
  ; 49,283 -> 226,396
  (road city-loc-10 city-loc-9)
  (= (road-length city-loc-10 city-loc-9) 21)
  (= (fuel-demand city-loc-10 city-loc-9) 42)
  ; 226,396 -> 49,283
  (road city-loc-9 city-loc-10)
  (= (road-length city-loc-9 city-loc-10) 21)
  (= (fuel-demand city-loc-9 city-loc-10) 42)
  ; 14,77 -> 147,230
  (road city-loc-11 city-loc-6)
  (= (road-length city-loc-11 city-loc-6) 21)
  (= (fuel-demand city-loc-11 city-loc-6) 41)
  ; 147,230 -> 14,77
  (road city-loc-6 city-loc-11)
  (= (road-length city-loc-6 city-loc-11) 21)
  (= (fuel-demand city-loc-6 city-loc-11) 41)
  ; 14,77 -> 49,283
  (road city-loc-11 city-loc-10)
  (= (road-length city-loc-11 city-loc-10) 21)
  (= (fuel-demand city-loc-11 city-loc-10) 42)
  ; 49,283 -> 14,77
  (road city-loc-10 city-loc-11)
  (= (road-length city-loc-10 city-loc-11) 21)
  (= (fuel-demand city-loc-10 city-loc-11) 42)
  (at package-1 city-loc-4)
  (at package-2 city-loc-7)
  (at package-3 city-loc-7)
  (at package-4 city-loc-5)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-3)
  (capacity truck-1 capacity-2)
  (= (fuel-left truck-1) 259)
  (= (fuel-max truck-1) 259)
  (at truck-2 city-loc-7)
  (capacity truck-2 capacity-2)
  (= (fuel-left truck-2) 259)
  (= (fuel-max truck-2) 259)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-8))
  (preference delivery-2 (at package-2 city-loc-5))
  (preference delivery-3 (at package-3 city-loc-11))
  (preference delivery-4 (at package-4 city-loc-6))
 ))
 (:metric maximize
   (- 329
     (+ (total-cost)
       (* (is-violated delivery-1) 58)
       (* (is-violated delivery-2) 74)
       (* (is-violated delivery-3) 83)
       (* (is-violated delivery-4) 114)
     )
   )
 )
)
