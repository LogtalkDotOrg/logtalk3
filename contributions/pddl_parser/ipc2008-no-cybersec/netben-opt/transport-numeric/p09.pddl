; Transport city-netbenefit-0petrol-station-14nodes-1000size-3degree-100mindistance-2trucks-6packagespercity-2008seed

(define (problem transport-city-netbenefit-0petrol-station-14nodes-1000size-3degree-100mindistance-2trucks-6packagespercity-2008seed)
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
  ; 257,5 -> 6,60
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 26)
  (= (fuel-demand city-loc-4 city-loc-2) 52)
  ; 6,60 -> 257,5
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 26)
  (= (fuel-demand city-loc-2 city-loc-4) 52)
  ; 559,565 -> 659,497
  (road city-loc-6 city-loc-3)
  (= (road-length city-loc-6 city-loc-3) 13)
  (= (fuel-demand city-loc-6 city-loc-3) 25)
  ; 659,497 -> 559,565
  (road city-loc-3 city-loc-6)
  (= (road-length city-loc-3 city-loc-6) 13)
  (= (fuel-demand city-loc-3 city-loc-6) 25)
  ; 347,149 -> 257,5
  (road city-loc-7 city-loc-4)
  (= (road-length city-loc-7 city-loc-4) 17)
  (= (fuel-demand city-loc-7 city-loc-4) 34)
  ; 257,5 -> 347,149
  (road city-loc-4 city-loc-7)
  (= (road-length city-loc-4 city-loc-7) 17)
  (= (fuel-demand city-loc-4 city-loc-7) 34)
  ; 347,149 -> 245,346
  (road city-loc-7 city-loc-5)
  (= (road-length city-loc-7 city-loc-5) 23)
  (= (fuel-demand city-loc-7 city-loc-5) 45)
  ; 245,346 -> 347,149
  (road city-loc-5 city-loc-7)
  (= (road-length city-loc-5 city-loc-7) 23)
  (= (fuel-demand city-loc-5 city-loc-7) 45)
  ; 336,475 -> 245,346
  (road city-loc-8 city-loc-5)
  (= (road-length city-loc-8 city-loc-5) 16)
  (= (fuel-demand city-loc-8 city-loc-5) 32)
  ; 245,346 -> 336,475
  (road city-loc-5 city-loc-8)
  (= (road-length city-loc-5 city-loc-8) 16)
  (= (fuel-demand city-loc-5 city-loc-8) 32)
  ; 336,475 -> 559,565
  (road city-loc-8 city-loc-6)
  (= (road-length city-loc-8 city-loc-6) 24)
  (= (fuel-demand city-loc-8 city-loc-6) 48)
  ; 559,565 -> 336,475
  (road city-loc-6 city-loc-8)
  (= (road-length city-loc-6 city-loc-8) 24)
  (= (fuel-demand city-loc-6 city-loc-8) 48)
  ; 170,709 -> 336,475
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 29)
  (= (fuel-demand city-loc-9 city-loc-8) 58)
  ; 336,475 -> 170,709
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 29)
  (= (fuel-demand city-loc-8 city-loc-9) 58)
  ; 521,375 -> 659,497
  (road city-loc-10 city-loc-3)
  (= (road-length city-loc-10 city-loc-3) 19)
  (= (fuel-demand city-loc-10 city-loc-3) 37)
  ; 659,497 -> 521,375
  (road city-loc-3 city-loc-10)
  (= (road-length city-loc-3 city-loc-10) 19)
  (= (fuel-demand city-loc-3 city-loc-10) 37)
  ; 521,375 -> 245,346
  (road city-loc-10 city-loc-5)
  (= (road-length city-loc-10 city-loc-5) 28)
  (= (fuel-demand city-loc-10 city-loc-5) 56)
  ; 245,346 -> 521,375
  (road city-loc-5 city-loc-10)
  (= (road-length city-loc-5 city-loc-10) 28)
  (= (fuel-demand city-loc-5 city-loc-10) 56)
  ; 521,375 -> 559,565
  (road city-loc-10 city-loc-6)
  (= (road-length city-loc-10 city-loc-6) 20)
  (= (fuel-demand city-loc-10 city-loc-6) 39)
  ; 559,565 -> 521,375
  (road city-loc-6 city-loc-10)
  (= (road-length city-loc-6 city-loc-10) 20)
  (= (fuel-demand city-loc-6 city-loc-10) 39)
  ; 521,375 -> 347,149
  (road city-loc-10 city-loc-7)
  (= (road-length city-loc-10 city-loc-7) 29)
  (= (fuel-demand city-loc-10 city-loc-7) 57)
  ; 347,149 -> 521,375
  (road city-loc-7 city-loc-10)
  (= (road-length city-loc-7 city-loc-10) 29)
  (= (fuel-demand city-loc-7 city-loc-10) 57)
  ; 521,375 -> 336,475
  (road city-loc-10 city-loc-8)
  (= (road-length city-loc-10 city-loc-8) 21)
  (= (fuel-demand city-loc-10 city-loc-8) 42)
  ; 336,475 -> 521,375
  (road city-loc-8 city-loc-10)
  (= (road-length city-loc-8 city-loc-10) 21)
  (= (fuel-demand city-loc-8 city-loc-10) 42)
  ; 720,241 -> 659,497
  (road city-loc-12 city-loc-3)
  (= (road-length city-loc-12 city-loc-3) 27)
  (= (fuel-demand city-loc-12 city-loc-3) 53)
  ; 659,497 -> 720,241
  (road city-loc-3 city-loc-12)
  (= (road-length city-loc-3 city-loc-12) 27)
  (= (fuel-demand city-loc-3 city-loc-12) 53)
  ; 720,241 -> 521,375
  (road city-loc-12 city-loc-10)
  (= (road-length city-loc-12 city-loc-10) 24)
  (= (fuel-demand city-loc-12 city-loc-10) 48)
  ; 521,375 -> 720,241
  (road city-loc-10 city-loc-12)
  (= (road-length city-loc-10 city-loc-12) 24)
  (= (fuel-demand city-loc-10 city-loc-12) 48)
  ; 720,241 -> 701,0
  (road city-loc-12 city-loc-11)
  (= (road-length city-loc-12 city-loc-11) 25)
  (= (fuel-demand city-loc-12 city-loc-11) 49)
  ; 701,0 -> 720,241
  (road city-loc-11 city-loc-12)
  (= (road-length city-loc-11 city-loc-12) 25)
  (= (fuel-demand city-loc-11 city-loc-12) 49)
  ; 630,722 -> 748,863
  (road city-loc-13 city-loc-1)
  (= (road-length city-loc-13 city-loc-1) 19)
  (= (fuel-demand city-loc-13 city-loc-1) 37)
  ; 748,863 -> 630,722
  (road city-loc-1 city-loc-13)
  (= (road-length city-loc-1 city-loc-13) 19)
  (= (fuel-demand city-loc-1 city-loc-13) 37)
  ; 630,722 -> 659,497
  (road city-loc-13 city-loc-3)
  (= (road-length city-loc-13 city-loc-3) 23)
  (= (fuel-demand city-loc-13 city-loc-3) 46)
  ; 659,497 -> 630,722
  (road city-loc-3 city-loc-13)
  (= (road-length city-loc-3 city-loc-13) 23)
  (= (fuel-demand city-loc-3 city-loc-13) 46)
  ; 630,722 -> 559,565
  (road city-loc-13 city-loc-6)
  (= (road-length city-loc-13 city-loc-6) 18)
  (= (fuel-demand city-loc-13 city-loc-6) 35)
  ; 559,565 -> 630,722
  (road city-loc-6 city-loc-13)
  (= (road-length city-loc-6 city-loc-13) 18)
  (= (fuel-demand city-loc-6 city-loc-13) 35)
  ; 120,854 -> 170,709
  (road city-loc-14 city-loc-9)
  (= (road-length city-loc-14 city-loc-9) 16)
  (= (fuel-demand city-loc-14 city-loc-9) 31)
  ; 170,709 -> 120,854
  (road city-loc-9 city-loc-14)
  (= (road-length city-loc-9 city-loc-14) 16)
  (= (fuel-demand city-loc-9 city-loc-14) 31)
  (at package-1 city-loc-6)
  (at package-2 city-loc-4)
  (at package-3 city-loc-3)
  (at package-4 city-loc-8)
  (at package-5 city-loc-5)
  (at package-6 city-loc-9)
  (at truck-1 city-loc-11)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 555)
  (= (fuel-max truck-1) 555)
  (at truck-2 city-loc-10)
  (capacity truck-2 capacity-3)
  (= (fuel-left truck-2) 555)
  (= (fuel-max truck-2) 555)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-5))
  (preference delivery-2 (at package-2 city-loc-8))
  (preference delivery-3 (at package-3 city-loc-6))
  (preference delivery-4 (at package-4 city-loc-4))
  (preference delivery-5 (at package-5 city-loc-9))
  (preference delivery-6 (at package-6 city-loc-11))
 ))
 (:metric maximize
   (- 494
     (+ (total-cost)
       (* (is-violated delivery-1) 58)
       (* (is-violated delivery-2) 78)
       (* (is-violated delivery-3) 136)
       (* (is-violated delivery-4) 64)
       (* (is-violated delivery-5) 57)
       (* (is-violated delivery-6) 101)
     )
   )
 )
)
