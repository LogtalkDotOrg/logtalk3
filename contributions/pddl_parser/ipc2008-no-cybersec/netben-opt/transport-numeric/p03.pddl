; Transport city-netbenefit-0petrol-station-8nodes-1000size-3degree-100mindistance-2trucks-3packagespercity-2008seed

(define (problem transport-city-netbenefit-0petrol-station-8nodes-1000size-3degree-100mindistance-2trucks-3packagespercity-2008seed)
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
  ; 748,385 -> 890,543
  (road city-loc-3 city-loc-1)
  (= (road-length city-loc-3 city-loc-1) 22)
  (= (fuel-demand city-loc-3 city-loc-1) 43)
  ; 890,543 -> 748,385
  (road city-loc-1 city-loc-3)
  (= (road-length city-loc-1 city-loc-3) 22)
  (= (fuel-demand city-loc-1 city-loc-3) 43)
  ; 912,799 -> 890,543
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 26)
  (= (fuel-demand city-loc-4 city-loc-1) 52)
  ; 890,543 -> 912,799
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 26)
  (= (fuel-demand city-loc-1 city-loc-4) 52)
  ; 977,899 -> 890,543
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 37)
  (= (fuel-demand city-loc-5 city-loc-1) 74)
  ; 890,543 -> 977,899
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 37)
  (= (fuel-demand city-loc-1 city-loc-5) 74)
  ; 977,899 -> 912,799
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 12)
  (= (fuel-demand city-loc-5 city-loc-4) 24)
  ; 912,799 -> 977,899
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 12)
  (= (fuel-demand city-loc-4 city-loc-5) 24)
  ; 456,221 -> 384,50
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 19)
  (= (fuel-demand city-loc-6 city-loc-2) 38)
  ; 384,50 -> 456,221
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 19)
  (= (fuel-demand city-loc-2 city-loc-6) 38)
  ; 456,221 -> 748,385
  (road city-loc-6 city-loc-3)
  (= (road-length city-loc-6 city-loc-3) 34)
  (= (fuel-demand city-loc-6 city-loc-3) 67)
  ; 748,385 -> 456,221
  (road city-loc-3 city-loc-6)
  (= (road-length city-loc-3 city-loc-6) 34)
  (= (fuel-demand city-loc-3 city-loc-6) 67)
  ; 742,542 -> 890,543
  (road city-loc-7 city-loc-1)
  (= (road-length city-loc-7 city-loc-1) 15)
  (= (fuel-demand city-loc-7 city-loc-1) 30)
  ; 890,543 -> 742,542
  (road city-loc-1 city-loc-7)
  (= (road-length city-loc-1 city-loc-7) 15)
  (= (fuel-demand city-loc-1 city-loc-7) 30)
  ; 742,542 -> 748,385
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 16)
  (= (fuel-demand city-loc-7 city-loc-3) 32)
  ; 748,385 -> 742,542
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 16)
  (= (fuel-demand city-loc-3 city-loc-7) 32)
  ; 742,542 -> 912,799
  (road city-loc-7 city-loc-4)
  (= (road-length city-loc-7 city-loc-4) 31)
  (= (fuel-demand city-loc-7 city-loc-4) 62)
  ; 912,799 -> 742,542
  (road city-loc-4 city-loc-7)
  (= (road-length city-loc-4 city-loc-7) 31)
  (= (fuel-demand city-loc-4 city-loc-7) 62)
  ; 564,783 -> 890,543
  (road city-loc-8 city-loc-1)
  (= (road-length city-loc-8 city-loc-1) 41)
  (= (fuel-demand city-loc-8 city-loc-1) 81)
  ; 890,543 -> 564,783
  (road city-loc-1 city-loc-8)
  (= (road-length city-loc-1 city-loc-8) 41)
  (= (fuel-demand city-loc-1 city-loc-8) 81)
  ; 564,783 -> 912,799
  (road city-loc-8 city-loc-4)
  (= (road-length city-loc-8 city-loc-4) 35)
  (= (fuel-demand city-loc-8 city-loc-4) 70)
  ; 912,799 -> 564,783
  (road city-loc-4 city-loc-8)
  (= (road-length city-loc-4 city-loc-8) 35)
  (= (fuel-demand city-loc-4 city-loc-8) 70)
  ; 564,783 -> 742,542
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 30)
  (= (fuel-demand city-loc-8 city-loc-7) 60)
  ; 742,542 -> 564,783
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 30)
  (= (fuel-demand city-loc-7 city-loc-8) 60)
  (at package-1 city-loc-3)
  (at package-2 city-loc-4)
  (at package-3 city-loc-5)
  (at truck-1 city-loc-5)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 323)
  (= (fuel-max truck-1) 323)
  (at truck-2 city-loc-2)
  (capacity truck-2 capacity-3)
  (= (fuel-left truck-2) 323)
  (= (fuel-max truck-2) 323)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-6))
  (preference delivery-2 (at package-2 city-loc-8))
  (preference delivery-3 (at package-3 city-loc-8))
 ))
 (:metric maximize
   (- 276
     (+ (total-cost)
       (* (is-violated delivery-1) 144)
       (* (is-violated delivery-2) 76)
       (* (is-violated delivery-3) 56)
     )
   )
 )
)
