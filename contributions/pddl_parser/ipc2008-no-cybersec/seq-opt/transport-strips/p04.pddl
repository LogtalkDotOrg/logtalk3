; Transport city-sequential-12nodes-1000size-3degree-100mindistance-2trucks-5packages-2008seed

(define (problem transport-city-sequential-12nodes-1000size-3degree-100mindistance-2trucks-5packages-2008seed)
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
  ; 748,385 -> 890,543
  (road city-loc-3 city-loc-1)
  (= (road-length city-loc-3 city-loc-1) 22)
  ; 890,543 -> 748,385
  (road city-loc-1 city-loc-3)
  (= (road-length city-loc-1 city-loc-3) 22)
  ; 912,799 -> 890,543
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 26)
  ; 890,543 -> 912,799
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 26)
  ; 977,899 -> 912,799
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 12)
  ; 912,799 -> 977,899
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 12)
  ; 456,221 -> 384,50
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 19)
  ; 384,50 -> 456,221
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 19)
  ; 456,221 -> 748,385
  (road city-loc-6 city-loc-3)
  (= (road-length city-loc-6 city-loc-3) 34)
  ; 748,385 -> 456,221
  (road city-loc-3 city-loc-6)
  (= (road-length city-loc-3 city-loc-6) 34)
  ; 742,542 -> 890,543
  (road city-loc-7 city-loc-1)
  (= (road-length city-loc-7 city-loc-1) 15)
  ; 890,543 -> 742,542
  (road city-loc-1 city-loc-7)
  (= (road-length city-loc-1 city-loc-7) 15)
  ; 742,542 -> 748,385
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 16)
  ; 748,385 -> 742,542
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 16)
  ; 742,542 -> 912,799
  (road city-loc-7 city-loc-4)
  (= (road-length city-loc-7 city-loc-4) 31)
  ; 912,799 -> 742,542
  (road city-loc-4 city-loc-7)
  (= (road-length city-loc-4 city-loc-7) 31)
  ; 564,783 -> 742,542
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 30)
  ; 742,542 -> 564,783
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 30)
  ; 273,425 -> 456,221
  (road city-loc-9 city-loc-6)
  (= (road-length city-loc-9 city-loc-6) 28)
  ; 456,221 -> 273,425
  (road city-loc-6 city-loc-9)
  (= (road-length city-loc-6 city-loc-9) 28)
  ; 566,552 -> 890,543
  (road city-loc-10 city-loc-1)
  (= (road-length city-loc-10 city-loc-1) 33)
  ; 890,543 -> 566,552
  (road city-loc-1 city-loc-10)
  (= (road-length city-loc-1 city-loc-10) 33)
  ; 566,552 -> 748,385
  (road city-loc-10 city-loc-3)
  (= (road-length city-loc-10 city-loc-3) 25)
  ; 748,385 -> 566,552
  (road city-loc-3 city-loc-10)
  (= (road-length city-loc-3 city-loc-10) 25)
  ; 566,552 -> 742,542
  (road city-loc-10 city-loc-7)
  (= (road-length city-loc-10 city-loc-7) 18)
  ; 742,542 -> 566,552
  (road city-loc-7 city-loc-10)
  (= (road-length city-loc-7 city-loc-10) 18)
  ; 566,552 -> 564,783
  (road city-loc-10 city-loc-8)
  (= (road-length city-loc-10 city-loc-8) 24)
  ; 564,783 -> 566,552
  (road city-loc-8 city-loc-10)
  (= (road-length city-loc-8 city-loc-10) 24)
  ; 566,552 -> 273,425
  (road city-loc-10 city-loc-9)
  (= (road-length city-loc-10 city-loc-9) 32)
  ; 273,425 -> 566,552
  (road city-loc-9 city-loc-10)
  (= (road-length city-loc-9 city-loc-10) 32)
  ; 174,643 -> 273,425
  (road city-loc-11 city-loc-9)
  (= (road-length city-loc-11 city-loc-9) 24)
  ; 273,425 -> 174,643
  (road city-loc-9 city-loc-11)
  (= (road-length city-loc-9 city-loc-11) 24)
  ; 930,259 -> 890,543
  (road city-loc-12 city-loc-1)
  (= (road-length city-loc-12 city-loc-1) 29)
  ; 890,543 -> 930,259
  (road city-loc-1 city-loc-12)
  (= (road-length city-loc-1 city-loc-12) 29)
  ; 930,259 -> 748,385
  (road city-loc-12 city-loc-3)
  (= (road-length city-loc-12 city-loc-3) 23)
  ; 748,385 -> 930,259
  (road city-loc-3 city-loc-12)
  (= (road-length city-loc-3 city-loc-12) 23)
  (at package-1 city-loc-1)
  (at package-2 city-loc-8)
  (at package-3 city-loc-10)
  (at package-4 city-loc-11)
  (at package-5 city-loc-4)
  (at truck-1 city-loc-2)
  (capacity truck-1 capacity-3)
  (at truck-2 city-loc-6)
  (capacity truck-2 capacity-4)
 )
 (:goal (and
  (at package-1 city-loc-9)
  (at package-2 city-loc-1)
  (at package-3 city-loc-5)
  (at package-4 city-loc-10)
  (at package-5 city-loc-1)
 ))
 (:metric minimize (total-cost))
)
