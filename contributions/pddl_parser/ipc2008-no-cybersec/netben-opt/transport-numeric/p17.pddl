; Transport city-netbenefit-1petrol-station-12nodes-1000size-3degree-100mindistance-2trucks-5packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-12nodes-1000size-3degree-100mindistance-2trucks-5packagespercity-4016seed)
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
  ; 226,396 -> 147,230
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 19)
  (= (fuel-demand city-loc-4 city-loc-1) 37)
  ; 147,230 -> 226,396
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 19)
  (= (fuel-demand city-loc-1 city-loc-4) 37)
  ; 49,283 -> 147,230
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 12)
  (= (fuel-demand city-loc-5 city-loc-1) 23)
  ; 147,230 -> 49,283
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 12)
  (= (fuel-demand city-loc-1 city-loc-5) 23)
  ; 49,283 -> 226,396
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 21)
  (= (fuel-demand city-loc-5 city-loc-4) 42)
  ; 226,396 -> 49,283
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 21)
  (= (fuel-demand city-loc-4 city-loc-5) 42)
  ; 14,77 -> 147,230
  (road city-loc-6 city-loc-1)
  (= (road-length city-loc-6 city-loc-1) 21)
  (= (fuel-demand city-loc-6 city-loc-1) 41)
  ; 147,230 -> 14,77
  (road city-loc-1 city-loc-6)
  (= (road-length city-loc-1 city-loc-6) 21)
  (= (fuel-demand city-loc-1 city-loc-6) 41)
  ; 14,77 -> 49,283
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 21)
  (= (fuel-demand city-loc-6 city-loc-5) 42)
  ; 49,283 -> 14,77
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 21)
  (= (fuel-demand city-loc-5 city-loc-6) 42)
  ; 340,590 -> 310,758
  (road city-loc-7 city-loc-2)
  (= (road-length city-loc-7 city-loc-2) 18)
  (= (fuel-demand city-loc-7 city-loc-2) 35)
  ; 310,758 -> 340,590
  (road city-loc-2 city-loc-7)
  (= (road-length city-loc-2 city-loc-7) 18)
  (= (fuel-demand city-loc-2 city-loc-7) 35)
  ; 340,590 -> 595,480
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 28)
  (= (fuel-demand city-loc-7 city-loc-3) 56)
  ; 595,480 -> 340,590
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 28)
  (= (fuel-demand city-loc-3 city-loc-7) 56)
  ; 340,590 -> 226,396
  (road city-loc-7 city-loc-4)
  (= (road-length city-loc-7 city-loc-4) 23)
  (= (fuel-demand city-loc-7 city-loc-4) 45)
  ; 226,396 -> 340,590
  (road city-loc-4 city-loc-7)
  (= (road-length city-loc-4 city-loc-7) 23)
  (= (fuel-demand city-loc-4 city-loc-7) 45)
  ; 621,371 -> 595,480
  (road city-loc-8 city-loc-3)
  (= (road-length city-loc-8 city-loc-3) 12)
  (= (fuel-demand city-loc-8 city-loc-3) 23)
  ; 595,480 -> 621,371
  (road city-loc-3 city-loc-8)
  (= (road-length city-loc-3 city-loc-8) 12)
  (= (fuel-demand city-loc-3 city-loc-8) 23)
  ; 328,272 -> 147,230
  (road city-loc-9 city-loc-1)
  (= (road-length city-loc-9 city-loc-1) 19)
  (= (fuel-demand city-loc-9 city-loc-1) 38)
  ; 147,230 -> 328,272
  (road city-loc-1 city-loc-9)
  (= (road-length city-loc-1 city-loc-9) 19)
  (= (fuel-demand city-loc-1 city-loc-9) 38)
  ; 328,272 -> 595,480
  (road city-loc-9 city-loc-3)
  (= (road-length city-loc-9 city-loc-3) 34)
  (= (fuel-demand city-loc-9 city-loc-3) 68)
  ; 595,480 -> 328,272
  (road city-loc-3 city-loc-9)
  (= (road-length city-loc-3 city-loc-9) 34)
  (= (fuel-demand city-loc-3 city-loc-9) 68)
  ; 328,272 -> 226,396
  (road city-loc-9 city-loc-4)
  (= (road-length city-loc-9 city-loc-4) 17)
  (= (fuel-demand city-loc-9 city-loc-4) 33)
  ; 226,396 -> 328,272
  (road city-loc-4 city-loc-9)
  (= (road-length city-loc-4 city-loc-9) 17)
  (= (fuel-demand city-loc-4 city-loc-9) 33)
  ; 328,272 -> 49,283
  (road city-loc-9 city-loc-5)
  (= (road-length city-loc-9 city-loc-5) 28)
  (= (fuel-demand city-loc-9 city-loc-5) 56)
  ; 49,283 -> 328,272
  (road city-loc-5 city-loc-9)
  (= (road-length city-loc-5 city-loc-9) 28)
  (= (fuel-demand city-loc-5 city-loc-9) 56)
  ; 328,272 -> 340,590
  (road city-loc-9 city-loc-7)
  (= (road-length city-loc-9 city-loc-7) 32)
  (= (fuel-demand city-loc-9 city-loc-7) 64)
  ; 340,590 -> 328,272
  (road city-loc-7 city-loc-9)
  (= (road-length city-loc-7 city-loc-9) 32)
  (= (fuel-demand city-loc-7 city-loc-9) 64)
  ; 328,272 -> 621,371
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 31)
  (= (fuel-demand city-loc-9 city-loc-8) 62)
  ; 621,371 -> 328,272
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 31)
  (= (fuel-demand city-loc-8 city-loc-9) 62)
  ; 34,585 -> 310,758
  (road city-loc-10 city-loc-2)
  (= (road-length city-loc-10 city-loc-2) 33)
  (= (fuel-demand city-loc-10 city-loc-2) 66)
  ; 310,758 -> 34,585
  (road city-loc-2 city-loc-10)
  (= (road-length city-loc-2 city-loc-10) 33)
  (= (fuel-demand city-loc-2 city-loc-10) 66)
  ; 34,585 -> 226,396
  (road city-loc-10 city-loc-4)
  (= (road-length city-loc-10 city-loc-4) 27)
  (= (fuel-demand city-loc-10 city-loc-4) 54)
  ; 226,396 -> 34,585
  (road city-loc-4 city-loc-10)
  (= (road-length city-loc-4 city-loc-10) 27)
  (= (fuel-demand city-loc-4 city-loc-10) 54)
  ; 34,585 -> 49,283
  (road city-loc-10 city-loc-5)
  (= (road-length city-loc-10 city-loc-5) 31)
  (= (fuel-demand city-loc-10 city-loc-5) 61)
  ; 49,283 -> 34,585
  (road city-loc-5 city-loc-10)
  (= (road-length city-loc-5 city-loc-10) 31)
  (= (fuel-demand city-loc-5 city-loc-10) 61)
  ; 34,585 -> 340,590
  (road city-loc-10 city-loc-7)
  (= (road-length city-loc-10 city-loc-7) 31)
  (= (fuel-demand city-loc-10 city-loc-7) 62)
  ; 340,590 -> 34,585
  (road city-loc-7 city-loc-10)
  (= (road-length city-loc-7 city-loc-10) 31)
  (= (fuel-demand city-loc-7 city-loc-10) 62)
  ; 677,552 -> 595,480
  (road city-loc-11 city-loc-3)
  (= (road-length city-loc-11 city-loc-3) 11)
  (= (fuel-demand city-loc-11 city-loc-3) 22)
  ; 595,480 -> 677,552
  (road city-loc-3 city-loc-11)
  (= (road-length city-loc-3 city-loc-11) 11)
  (= (fuel-demand city-loc-3 city-loc-11) 22)
  ; 677,552 -> 621,371
  (road city-loc-11 city-loc-8)
  (= (road-length city-loc-11 city-loc-8) 19)
  (= (fuel-demand city-loc-11 city-loc-8) 38)
  ; 621,371 -> 677,552
  (road city-loc-8 city-loc-11)
  (= (road-length city-loc-8 city-loc-11) 19)
  (= (fuel-demand city-loc-8 city-loc-11) 38)
  ; 403,969 -> 310,758
  (road city-loc-12 city-loc-2)
  (= (road-length city-loc-12 city-loc-2) 24)
  (= (fuel-demand city-loc-12 city-loc-2) 47)
  ; 310,758 -> 403,969
  (road city-loc-2 city-loc-12)
  (= (road-length city-loc-2 city-loc-12) 24)
  (= (fuel-demand city-loc-2 city-loc-12) 47)
  (at package-1 city-loc-7)
  (at package-2 city-loc-1)
  (at package-3 city-loc-3)
  (at package-4 city-loc-4)
  (at package-5 city-loc-8)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-9)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 357)
  (= (fuel-max truck-1) 357)
  (at truck-2 city-loc-3)
  (capacity truck-2 capacity-4)
  (= (fuel-left truck-2) 357)
  (= (fuel-max truck-2) 357)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-4))
  (preference delivery-2 (at package-2 city-loc-9))
  (preference delivery-3 (at package-3 city-loc-2))
  (preference delivery-4 (at package-4 city-loc-5))
  (preference delivery-5 (at package-5 city-loc-1))
 ))
 (:metric maximize
   (- 571
     (+ (total-cost)
       (* (is-violated delivery-1) 98)
       (* (is-violated delivery-2) 130)
       (* (is-violated delivery-3) 141)
       (* (is-violated delivery-4) 105)
       (* (is-violated delivery-5) 97)
     )
   )
 )
)
