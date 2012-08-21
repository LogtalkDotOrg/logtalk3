; Transport city-sequential-21nodes-1000size-4degree-100mindistance-3trucks-8packages-2008seed

(define (problem transport-city-sequential-21nodes-1000size-4degree-100mindistance-3trucks-8packages-2008seed)
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
  city-loc-16 - location
  city-loc-17 - location
  city-loc-18 - location
  city-loc-19 - location
  city-loc-20 - location
  city-loc-21 - location
  truck-1 - vehicle
  truck-2 - vehicle
  truck-3 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
  package-4 - package
  package-5 - package
  package-6 - package
  package-7 - package
  package-8 - package
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
  ; 273,425 -> 456,221
  (road city-loc-9 city-loc-6)
  (= (road-length city-loc-9 city-loc-6) 28)
  ; 456,221 -> 273,425
  (road city-loc-6 city-loc-9)
  (= (road-length city-loc-6 city-loc-9) 28)
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
  ; 55,605 -> 273,425
  (road city-loc-13 city-loc-9)
  (= (road-length city-loc-13 city-loc-9) 29)
  ; 273,425 -> 55,605
  (road city-loc-9 city-loc-13)
  (= (road-length city-loc-9 city-loc-13) 29)
  ; 55,605 -> 174,643
  (road city-loc-13 city-loc-11)
  (= (road-length city-loc-13 city-loc-11) 13)
  ; 174,643 -> 55,605
  (road city-loc-11 city-loc-13)
  (= (road-length city-loc-11 city-loc-13) 13)
  ; 803,858 -> 912,799
  (road city-loc-14 city-loc-4)
  (= (road-length city-loc-14 city-loc-4) 13)
  ; 912,799 -> 803,858
  (road city-loc-4 city-loc-14)
  (= (road-length city-loc-4 city-loc-14) 13)
  ; 803,858 -> 977,899
  (road city-loc-14 city-loc-5)
  (= (road-length city-loc-14 city-loc-5) 18)
  ; 977,899 -> 803,858
  (road city-loc-5 city-loc-14)
  (= (road-length city-loc-5 city-loc-14) 18)
  ; 803,858 -> 564,783
  (road city-loc-14 city-loc-8)
  (= (road-length city-loc-14 city-loc-8) 25)
  ; 564,783 -> 803,858
  (road city-loc-8 city-loc-14)
  (= (road-length city-loc-8 city-loc-14) 25)
  ; 263,567 -> 273,425
  (road city-loc-15 city-loc-9)
  (= (road-length city-loc-15 city-loc-9) 15)
  ; 273,425 -> 263,567
  (road city-loc-9 city-loc-15)
  (= (road-length city-loc-9 city-loc-15) 15)
  ; 263,567 -> 174,643
  (road city-loc-15 city-loc-11)
  (= (road-length city-loc-15 city-loc-11) 12)
  ; 174,643 -> 263,567
  (road city-loc-11 city-loc-15)
  (= (road-length city-loc-11 city-loc-15) 12)
  ; 263,567 -> 55,605
  (road city-loc-15 city-loc-13)
  (= (road-length city-loc-15 city-loc-13) 22)
  ; 55,605 -> 263,567
  (road city-loc-13 city-loc-15)
  (= (road-length city-loc-13 city-loc-15) 22)
  ; 128,791 -> 174,643
  (road city-loc-16 city-loc-11)
  (= (road-length city-loc-16 city-loc-11) 16)
  ; 174,643 -> 128,791
  (road city-loc-11 city-loc-16)
  (= (road-length city-loc-11 city-loc-16) 16)
  ; 128,791 -> 55,605
  (road city-loc-16 city-loc-13)
  (= (road-length city-loc-16 city-loc-13) 20)
  ; 55,605 -> 128,791
  (road city-loc-13 city-loc-16)
  (= (road-length city-loc-13 city-loc-16) 20)
  ; 128,791 -> 263,567
  (road city-loc-16 city-loc-15)
  (= (road-length city-loc-16 city-loc-15) 27)
  ; 263,567 -> 128,791
  (road city-loc-15 city-loc-16)
  (= (road-length city-loc-15 city-loc-16) 27)
  ; 426,706 -> 564,783
  (road city-loc-17 city-loc-8)
  (= (road-length city-loc-17 city-loc-8) 16)
  ; 564,783 -> 426,706
  (road city-loc-8 city-loc-17)
  (= (road-length city-loc-8 city-loc-17) 16)
  ; 426,706 -> 566,552
  (road city-loc-17 city-loc-10)
  (= (road-length city-loc-17 city-loc-10) 21)
  ; 566,552 -> 426,706
  (road city-loc-10 city-loc-17)
  (= (road-length city-loc-10 city-loc-17) 21)
  ; 426,706 -> 174,643
  (road city-loc-17 city-loc-11)
  (= (road-length city-loc-17 city-loc-11) 26)
  ; 174,643 -> 426,706
  (road city-loc-11 city-loc-17)
  (= (road-length city-loc-11 city-loc-17) 26)
  ; 426,706 -> 263,567
  (road city-loc-17 city-loc-15)
  (= (road-length city-loc-17 city-loc-15) 22)
  ; 263,567 -> 426,706
  (road city-loc-15 city-loc-17)
  (= (road-length city-loc-15 city-loc-17) 22)
  ; 36,368 -> 273,425
  (road city-loc-18 city-loc-9)
  (= (road-length city-loc-18 city-loc-9) 25)
  ; 273,425 -> 36,368
  (road city-loc-9 city-loc-18)
  (= (road-length city-loc-9 city-loc-18) 25)
  ; 36,368 -> 55,605
  (road city-loc-18 city-loc-13)
  (= (road-length city-loc-18 city-loc-13) 24)
  ; 55,605 -> 36,368
  (road city-loc-13 city-loc-18)
  (= (road-length city-loc-13 city-loc-18) 24)
  ; 806,18 -> 930,259
  (road city-loc-19 city-loc-12)
  (= (road-length city-loc-19 city-loc-12) 28)
  ; 930,259 -> 806,18
  (road city-loc-12 city-loc-19)
  (= (road-length city-loc-12 city-loc-19) 28)
  ; 138,109 -> 384,50
  (road city-loc-20 city-loc-2)
  (= (road-length city-loc-20 city-loc-2) 26)
  ; 384,50 -> 138,109
  (road city-loc-2 city-loc-20)
  (= (road-length city-loc-2 city-loc-20) 26)
  ; 138,109 -> 36,368
  (road city-loc-20 city-loc-18)
  (= (road-length city-loc-20 city-loc-18) 28)
  ; 36,368 -> 138,109
  (road city-loc-18 city-loc-20)
  (= (road-length city-loc-18 city-loc-20) 28)
  ; 392,433 -> 456,221
  (road city-loc-21 city-loc-6)
  (= (road-length city-loc-21 city-loc-6) 23)
  ; 456,221 -> 392,433
  (road city-loc-6 city-loc-21)
  (= (road-length city-loc-6 city-loc-21) 23)
  ; 392,433 -> 273,425
  (road city-loc-21 city-loc-9)
  (= (road-length city-loc-21 city-loc-9) 12)
  ; 273,425 -> 392,433
  (road city-loc-9 city-loc-21)
  (= (road-length city-loc-9 city-loc-21) 12)
  ; 392,433 -> 566,552
  (road city-loc-21 city-loc-10)
  (= (road-length city-loc-21 city-loc-10) 22)
  ; 566,552 -> 392,433
  (road city-loc-10 city-loc-21)
  (= (road-length city-loc-10 city-loc-21) 22)
  ; 392,433 -> 263,567
  (road city-loc-21 city-loc-15)
  (= (road-length city-loc-21 city-loc-15) 19)
  ; 263,567 -> 392,433
  (road city-loc-15 city-loc-21)
  (= (road-length city-loc-15 city-loc-21) 19)
  ; 392,433 -> 426,706
  (road city-loc-21 city-loc-17)
  (= (road-length city-loc-21 city-loc-17) 28)
  ; 426,706 -> 392,433
  (road city-loc-17 city-loc-21)
  (= (road-length city-loc-17 city-loc-21) 28)
  (at package-1 city-loc-18)
  (at package-2 city-loc-19)
  (at package-3 city-loc-5)
  (at package-4 city-loc-19)
  (at package-5 city-loc-6)
  (at package-6 city-loc-19)
  (at package-7 city-loc-1)
  (at package-8 city-loc-7)
  (at truck-1 city-loc-1)
  (capacity truck-1 capacity-4)
  (at truck-2 city-loc-10)
  (capacity truck-2 capacity-4)
  (at truck-3 city-loc-17)
  (capacity truck-3 capacity-2)
 )
 (:goal (and
  (at package-1 city-loc-21)
  (at package-2 city-loc-5)
  (at package-3 city-loc-13)
  (at package-4 city-loc-16)
  (at package-5 city-loc-2)
  (at package-6 city-loc-4)
  (at package-7 city-loc-8)
  (at package-8 city-loc-19)
 ))
 (:metric minimize (total-cost))
)
