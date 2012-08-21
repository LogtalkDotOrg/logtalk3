; Transport city-sequential-27nodes-1000size-4degree-100mindistance-3trucks-10packages-2008seed

(define (problem transport-city-sequential-27nodes-1000size-4degree-100mindistance-3trucks-10packages-2008seed)
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
  city-loc-22 - location
  city-loc-23 - location
  city-loc-24 - location
  city-loc-25 - location
  city-loc-26 - location
  city-loc-27 - location
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
  package-9 - package
  package-10 - package
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
  ; 305,509 -> 437,605
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 17)
  ; 437,605 -> 305,509
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 17)
  ; 463,927 -> 339,962
  (road city-loc-7 city-loc-5)
  (= (road-length city-loc-7 city-loc-5) 13)
  ; 339,962 -> 463,927
  (road city-loc-5 city-loc-7)
  (= (road-length city-loc-5 city-loc-7) 13)
  ; 486,752 -> 437,605
  (road city-loc-8 city-loc-2)
  (= (road-length city-loc-8 city-loc-2) 16)
  ; 437,605 -> 486,752
  (road city-loc-2 city-loc-8)
  (= (road-length city-loc-2 city-loc-8) 16)
  ; 486,752 -> 339,962
  (road city-loc-8 city-loc-5)
  (= (road-length city-loc-8 city-loc-5) 26)
  ; 339,962 -> 486,752
  (road city-loc-5 city-loc-8)
  (= (road-length city-loc-5 city-loc-8) 26)
  ; 486,752 -> 463,927
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 18)
  ; 463,927 -> 486,752
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 18)
  ; 281,709 -> 437,605
  (road city-loc-9 city-loc-2)
  (= (road-length city-loc-9 city-loc-2) 19)
  ; 437,605 -> 281,709
  (road city-loc-2 city-loc-9)
  (= (road-length city-loc-2 city-loc-9) 19)
  ; 281,709 -> 305,509
  (road city-loc-9 city-loc-4)
  (= (road-length city-loc-9 city-loc-4) 21)
  ; 305,509 -> 281,709
  (road city-loc-4 city-loc-9)
  (= (road-length city-loc-4 city-loc-9) 21)
  ; 281,709 -> 339,962
  (road city-loc-9 city-loc-5)
  (= (road-length city-loc-9 city-loc-5) 26)
  ; 339,962 -> 281,709
  (road city-loc-5 city-loc-9)
  (= (road-length city-loc-5 city-loc-9) 26)
  ; 281,709 -> 486,752
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 21)
  ; 486,752 -> 281,709
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 21)
  ; 205,275 -> 305,509
  (road city-loc-10 city-loc-4)
  (= (road-length city-loc-10 city-loc-4) 26)
  ; 305,509 -> 205,275
  (road city-loc-4 city-loc-10)
  (= (road-length city-loc-4 city-loc-10) 26)
  ; 119,757 -> 281,709
  (road city-loc-11 city-loc-9)
  (= (road-length city-loc-11 city-loc-9) 17)
  ; 281,709 -> 119,757
  (road city-loc-9 city-loc-11)
  (= (road-length city-loc-9 city-loc-11) 17)
  ; 179,400 -> 305,509
  (road city-loc-12 city-loc-4)
  (= (road-length city-loc-12 city-loc-4) 17)
  ; 305,509 -> 179,400
  (road city-loc-4 city-loc-12)
  (= (road-length city-loc-4 city-loc-12) 17)
  ; 179,400 -> 205,275
  (road city-loc-12 city-loc-10)
  (= (road-length city-loc-12 city-loc-10) 13)
  ; 205,275 -> 179,400
  (road city-loc-10 city-loc-12)
  (= (road-length city-loc-10 city-loc-12) 13)
  ; 15,529 -> 119,757
  (road city-loc-13 city-loc-11)
  (= (road-length city-loc-13 city-loc-11) 26)
  ; 119,757 -> 15,529
  (road city-loc-11 city-loc-13)
  (= (road-length city-loc-11 city-loc-13) 26)
  ; 15,529 -> 179,400
  (road city-loc-13 city-loc-12)
  (= (road-length city-loc-13 city-loc-12) 21)
  ; 179,400 -> 15,529
  (road city-loc-12 city-loc-13)
  (= (road-length city-loc-12 city-loc-13) 21)
  ; 342,393 -> 437,605
  (road city-loc-14 city-loc-2)
  (= (road-length city-loc-14 city-loc-2) 24)
  ; 437,605 -> 342,393
  (road city-loc-2 city-loc-14)
  (= (road-length city-loc-2 city-loc-14) 24)
  ; 342,393 -> 497,244
  (road city-loc-14 city-loc-3)
  (= (road-length city-loc-14 city-loc-3) 22)
  ; 497,244 -> 342,393
  (road city-loc-3 city-loc-14)
  (= (road-length city-loc-3 city-loc-14) 22)
  ; 342,393 -> 305,509
  (road city-loc-14 city-loc-4)
  (= (road-length city-loc-14 city-loc-4) 13)
  ; 305,509 -> 342,393
  (road city-loc-4 city-loc-14)
  (= (road-length city-loc-4 city-loc-14) 13)
  ; 342,393 -> 205,275
  (road city-loc-14 city-loc-10)
  (= (road-length city-loc-14 city-loc-10) 19)
  ; 205,275 -> 342,393
  (road city-loc-10 city-loc-14)
  (= (road-length city-loc-10 city-loc-14) 19)
  ; 342,393 -> 179,400
  (road city-loc-14 city-loc-12)
  (= (road-length city-loc-14 city-loc-12) 17)
  ; 179,400 -> 342,393
  (road city-loc-12 city-loc-14)
  (= (road-length city-loc-12 city-loc-14) 17)
  ; 612,304 -> 497,244
  (road city-loc-15 city-loc-3)
  (= (road-length city-loc-15 city-loc-3) 13)
  ; 497,244 -> 612,304
  (road city-loc-3 city-loc-15)
  (= (road-length city-loc-3 city-loc-15) 13)
  ; 188,934 -> 339,962
  (road city-loc-16 city-loc-5)
  (= (road-length city-loc-16 city-loc-5) 16)
  ; 339,962 -> 188,934
  (road city-loc-5 city-loc-16)
  (= (road-length city-loc-5 city-loc-16) 16)
  ; 188,934 -> 281,709
  (road city-loc-16 city-loc-9)
  (= (road-length city-loc-16 city-loc-9) 25)
  ; 281,709 -> 188,934
  (road city-loc-9 city-loc-16)
  (= (road-length city-loc-9 city-loc-16) 25)
  ; 188,934 -> 119,757
  (road city-loc-16 city-loc-11)
  (= (road-length city-loc-16 city-loc-11) 19)
  ; 119,757 -> 188,934
  (road city-loc-11 city-loc-16)
  (= (road-length city-loc-11 city-loc-16) 19)
  ; 660,909 -> 463,927
  (road city-loc-18 city-loc-7)
  (= (road-length city-loc-18 city-loc-7) 20)
  ; 463,927 -> 660,909
  (road city-loc-7 city-loc-18)
  (= (road-length city-loc-7 city-loc-18) 20)
  ; 660,909 -> 486,752
  (road city-loc-18 city-loc-8)
  (= (road-length city-loc-18 city-loc-8) 24)
  ; 486,752 -> 660,909
  (road city-loc-8 city-loc-18)
  (= (road-length city-loc-8 city-loc-18) 24)
  ; 660,909 -> 776,948
  (road city-loc-18 city-loc-17)
  (= (road-length city-loc-18 city-loc-17) 13)
  ; 776,948 -> 660,909
  (road city-loc-17 city-loc-18)
  (= (road-length city-loc-17 city-loc-18) 13)
  ; 916,162 -> 731,24
  (road city-loc-19 city-loc-6)
  (= (road-length city-loc-19 city-loc-6) 24)
  ; 731,24 -> 916,162
  (road city-loc-6 city-loc-19)
  (= (road-length city-loc-6 city-loc-19) 24)
  ; 914,881 -> 776,948
  (road city-loc-20 city-loc-17)
  (= (road-length city-loc-20 city-loc-17) 16)
  ; 776,948 -> 914,881
  (road city-loc-17 city-loc-20)
  (= (road-length city-loc-17 city-loc-20) 16)
  ; 914,881 -> 660,909
  (road city-loc-20 city-loc-18)
  (= (road-length city-loc-20 city-loc-18) 26)
  ; 660,909 -> 914,881
  (road city-loc-18 city-loc-20)
  (= (road-length city-loc-18 city-loc-20) 26)
  ; 331,195 -> 497,244
  (road city-loc-21 city-loc-3)
  (= (road-length city-loc-21 city-loc-3) 18)
  ; 497,244 -> 331,195
  (road city-loc-3 city-loc-21)
  (= (road-length city-loc-3 city-loc-21) 18)
  ; 331,195 -> 205,275
  (road city-loc-21 city-loc-10)
  (= (road-length city-loc-21 city-loc-10) 15)
  ; 205,275 -> 331,195
  (road city-loc-10 city-loc-21)
  (= (road-length city-loc-10 city-loc-21) 15)
  ; 331,195 -> 179,400
  (road city-loc-21 city-loc-12)
  (= (road-length city-loc-21 city-loc-12) 26)
  ; 179,400 -> 331,195
  (road city-loc-12 city-loc-21)
  (= (road-length city-loc-12 city-loc-21) 26)
  ; 331,195 -> 342,393
  (road city-loc-21 city-loc-14)
  (= (road-length city-loc-21 city-loc-14) 20)
  ; 342,393 -> 331,195
  (road city-loc-14 city-loc-21)
  (= (road-length city-loc-14 city-loc-21) 20)
  ; 599,133 -> 497,244
  (road city-loc-22 city-loc-3)
  (= (road-length city-loc-22 city-loc-3) 16)
  ; 497,244 -> 599,133
  (road city-loc-3 city-loc-22)
  (= (road-length city-loc-3 city-loc-22) 16)
  ; 599,133 -> 731,24
  (road city-loc-22 city-loc-6)
  (= (road-length city-loc-22 city-loc-6) 18)
  ; 731,24 -> 599,133
  (road city-loc-6 city-loc-22)
  (= (road-length city-loc-6 city-loc-22) 18)
  ; 599,133 -> 612,304
  (road city-loc-22 city-loc-15)
  (= (road-length city-loc-22 city-loc-15) 18)
  ; 612,304 -> 599,133
  (road city-loc-15 city-loc-22)
  (= (road-length city-loc-15 city-loc-22) 18)
  ; 549,437 -> 437,605
  (road city-loc-23 city-loc-2)
  (= (road-length city-loc-23 city-loc-2) 21)
  ; 437,605 -> 549,437
  (road city-loc-2 city-loc-23)
  (= (road-length city-loc-2 city-loc-23) 21)
  ; 549,437 -> 497,244
  (road city-loc-23 city-loc-3)
  (= (road-length city-loc-23 city-loc-3) 20)
  ; 497,244 -> 549,437
  (road city-loc-3 city-loc-23)
  (= (road-length city-loc-3 city-loc-23) 20)
  ; 549,437 -> 305,509
  (road city-loc-23 city-loc-4)
  (= (road-length city-loc-23 city-loc-4) 26)
  ; 305,509 -> 549,437
  (road city-loc-4 city-loc-23)
  (= (road-length city-loc-4 city-loc-23) 26)
  ; 549,437 -> 342,393
  (road city-loc-23 city-loc-14)
  (= (road-length city-loc-23 city-loc-14) 22)
  ; 342,393 -> 549,437
  (road city-loc-14 city-loc-23)
  (= (road-length city-loc-14 city-loc-23) 22)
  ; 549,437 -> 612,304
  (road city-loc-23 city-loc-15)
  (= (road-length city-loc-23 city-loc-15) 15)
  ; 612,304 -> 549,437
  (road city-loc-15 city-loc-23)
  (= (road-length city-loc-15 city-loc-23) 15)
  ; 870,18 -> 731,24
  (road city-loc-24 city-loc-6)
  (= (road-length city-loc-24 city-loc-6) 14)
  ; 731,24 -> 870,18
  (road city-loc-6 city-loc-24)
  (= (road-length city-loc-6 city-loc-24) 14)
  ; 870,18 -> 916,162
  (road city-loc-24 city-loc-19)
  (= (road-length city-loc-24 city-loc-19) 16)
  ; 916,162 -> 870,18
  (road city-loc-19 city-loc-24)
  (= (road-length city-loc-19 city-loc-24) 16)
  ; 63,862 -> 119,757
  (road city-loc-25 city-loc-11)
  (= (road-length city-loc-25 city-loc-11) 12)
  ; 119,757 -> 63,862
  (road city-loc-11 city-loc-25)
  (= (road-length city-loc-11 city-loc-25) 12)
  ; 63,862 -> 188,934
  (road city-loc-25 city-loc-16)
  (= (road-length city-loc-25 city-loc-16) 15)
  ; 188,934 -> 63,862
  (road city-loc-16 city-loc-25)
  (= (road-length city-loc-16 city-loc-25) 15)
  ; 720,128 -> 497,244
  (road city-loc-26 city-loc-3)
  (= (road-length city-loc-26 city-loc-3) 26)
  ; 497,244 -> 720,128
  (road city-loc-3 city-loc-26)
  (= (road-length city-loc-3 city-loc-26) 26)
  ; 720,128 -> 731,24
  (road city-loc-26 city-loc-6)
  (= (road-length city-loc-26 city-loc-6) 11)
  ; 731,24 -> 720,128
  (road city-loc-6 city-loc-26)
  (= (road-length city-loc-6 city-loc-26) 11)
  ; 720,128 -> 612,304
  (road city-loc-26 city-loc-15)
  (= (road-length city-loc-26 city-loc-15) 21)
  ; 612,304 -> 720,128
  (road city-loc-15 city-loc-26)
  (= (road-length city-loc-15 city-loc-26) 21)
  ; 720,128 -> 916,162
  (road city-loc-26 city-loc-19)
  (= (road-length city-loc-26 city-loc-19) 20)
  ; 916,162 -> 720,128
  (road city-loc-19 city-loc-26)
  (= (road-length city-loc-19 city-loc-26) 20)
  ; 720,128 -> 599,133
  (road city-loc-26 city-loc-22)
  (= (road-length city-loc-26 city-loc-22) 13)
  ; 599,133 -> 720,128
  (road city-loc-22 city-loc-26)
  (= (road-length city-loc-22 city-loc-26) 13)
  ; 720,128 -> 870,18
  (road city-loc-26 city-loc-24)
  (= (road-length city-loc-26 city-loc-24) 19)
  ; 870,18 -> 720,128
  (road city-loc-24 city-loc-26)
  (= (road-length city-loc-24 city-loc-26) 19)
  ; 683,505 -> 820,551
  (road city-loc-27 city-loc-1)
  (= (road-length city-loc-27 city-loc-1) 15)
  ; 820,551 -> 683,505
  (road city-loc-1 city-loc-27)
  (= (road-length city-loc-1 city-loc-27) 15)
  ; 683,505 -> 612,304
  (road city-loc-27 city-loc-15)
  (= (road-length city-loc-27 city-loc-15) 22)
  ; 612,304 -> 683,505
  (road city-loc-15 city-loc-27)
  (= (road-length city-loc-15 city-loc-27) 22)
  ; 683,505 -> 549,437
  (road city-loc-27 city-loc-23)
  (= (road-length city-loc-27 city-loc-23) 15)
  ; 549,437 -> 683,505
  (road city-loc-23 city-loc-27)
  (= (road-length city-loc-23 city-loc-27) 15)
  (at package-1 city-loc-10)
  (at package-2 city-loc-1)
  (at package-3 city-loc-10)
  (at package-4 city-loc-2)
  (at package-5 city-loc-6)
  (at package-6 city-loc-7)
  (at package-7 city-loc-25)
  (at package-8 city-loc-24)
  (at package-9 city-loc-18)
  (at package-10 city-loc-22)
  (at truck-1 city-loc-15)
  (capacity truck-1 capacity-3)
  (at truck-2 city-loc-27)
  (capacity truck-2 capacity-3)
  (at truck-3 city-loc-18)
  (capacity truck-3 capacity-2)
 )
 (:goal (and
  (at package-1 city-loc-15)
  (at package-2 city-loc-2)
  (at package-3 city-loc-2)
  (at package-4 city-loc-17)
  (at package-5 city-loc-10)
  (at package-6 city-loc-14)
  (at package-7 city-loc-5)
  (at package-8 city-loc-5)
  (at package-9 city-loc-2)
  (at package-10 city-loc-7)
 ))
 (:metric minimize (total-cost))
)
