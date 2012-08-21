; Transport city-sequential-30nodes-1000size-4degree-100mindistance-3trucks-11packages-2008seed

(define (problem transport-city-sequential-30nodes-1000size-4degree-100mindistance-3trucks-11packages-2008seed)
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
  city-loc-28 - location
  city-loc-29 - location
  city-loc-30 - location
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
  package-11 - package
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
  ; 120,854 -> 200,669
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 21)
  ; 200,669 -> 120,854
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 21)
  ; 377,283 -> 490,285
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 12)
  ; 490,285 -> 377,283
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 12)
  ; 171,545 -> 200,669
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 13)
  ; 200,669 -> 171,545
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 13)
  ; 348,607 -> 200,669
  (road city-loc-7 city-loc-2)
  (= (road-length city-loc-7 city-loc-2) 16)
  ; 200,669 -> 348,607
  (road city-loc-2 city-loc-7)
  (= (road-length city-loc-2 city-loc-7) 16)
  ; 348,607 -> 171,545
  (road city-loc-7 city-loc-6)
  (= (road-length city-loc-7 city-loc-6) 19)
  ; 171,545 -> 348,607
  (road city-loc-6 city-loc-7)
  (= (road-length city-loc-6 city-loc-7) 19)
  ; 395,741 -> 200,669
  (road city-loc-8 city-loc-2)
  (= (road-length city-loc-8 city-loc-2) 21)
  ; 200,669 -> 395,741
  (road city-loc-2 city-loc-8)
  (= (road-length city-loc-2 city-loc-8) 21)
  ; 395,741 -> 630,722
  (road city-loc-8 city-loc-3)
  (= (road-length city-loc-8 city-loc-3) 24)
  ; 630,722 -> 395,741
  (road city-loc-3 city-loc-8)
  (= (road-length city-loc-3 city-loc-8) 24)
  ; 395,741 -> 348,607
  (road city-loc-8 city-loc-7)
  (= (road-length city-loc-8 city-loc-7) 15)
  ; 348,607 -> 395,741
  (road city-loc-7 city-loc-8)
  (= (road-length city-loc-7 city-loc-8) 15)
  ; 69,508 -> 200,669
  (road city-loc-11 city-loc-2)
  (= (road-length city-loc-11 city-loc-2) 21)
  ; 200,669 -> 69,508
  (road city-loc-2 city-loc-11)
  (= (road-length city-loc-2 city-loc-11) 21)
  ; 69,508 -> 171,545
  (road city-loc-11 city-loc-6)
  (= (road-length city-loc-11 city-loc-6) 11)
  ; 171,545 -> 69,508
  (road city-loc-6 city-loc-11)
  (= (road-length city-loc-6 city-loc-11) 11)
  ; 69,508 -> 71,275
  (road city-loc-11 city-loc-9)
  (= (road-length city-loc-11 city-loc-9) 24)
  ; 71,275 -> 69,508
  (road city-loc-9 city-loc-11)
  (= (road-length city-loc-9 city-loc-11) 24)
  ; 203,987 -> 120,854
  (road city-loc-12 city-loc-4)
  (= (road-length city-loc-12 city-loc-4) 16)
  ; 120,854 -> 203,987
  (road city-loc-4 city-loc-12)
  (= (road-length city-loc-4 city-loc-12) 16)
  ; 551,602 -> 630,722
  (road city-loc-14 city-loc-3)
  (= (road-length city-loc-14 city-loc-3) 15)
  ; 630,722 -> 551,602
  (road city-loc-3 city-loc-14)
  (= (road-length city-loc-3 city-loc-14) 15)
  ; 551,602 -> 348,607
  (road city-loc-14 city-loc-7)
  (= (road-length city-loc-14 city-loc-7) 21)
  ; 348,607 -> 551,602
  (road city-loc-7 city-loc-14)
  (= (road-length city-loc-7 city-loc-14) 21)
  ; 551,602 -> 395,741
  (road city-loc-14 city-loc-8)
  (= (road-length city-loc-14 city-loc-8) 21)
  ; 395,741 -> 551,602
  (road city-loc-8 city-loc-14)
  (= (road-length city-loc-8 city-loc-14) 21)
  ; 366,457 -> 490,285
  (road city-loc-15 city-loc-1)
  (= (road-length city-loc-15 city-loc-1) 22)
  ; 490,285 -> 366,457
  (road city-loc-1 city-loc-15)
  (= (road-length city-loc-1 city-loc-15) 22)
  ; 366,457 -> 377,283
  (road city-loc-15 city-loc-5)
  (= (road-length city-loc-15 city-loc-5) 18)
  ; 377,283 -> 366,457
  (road city-loc-5 city-loc-15)
  (= (road-length city-loc-5 city-loc-15) 18)
  ; 366,457 -> 171,545
  (road city-loc-15 city-loc-6)
  (= (road-length city-loc-15 city-loc-6) 22)
  ; 171,545 -> 366,457
  (road city-loc-6 city-loc-15)
  (= (road-length city-loc-6 city-loc-15) 22)
  ; 366,457 -> 348,607
  (road city-loc-15 city-loc-7)
  (= (road-length city-loc-15 city-loc-7) 16)
  ; 348,607 -> 366,457
  (road city-loc-7 city-loc-15)
  (= (road-length city-loc-7 city-loc-15) 16)
  ; 366,457 -> 551,602
  (road city-loc-15 city-loc-14)
  (= (road-length city-loc-15 city-loc-14) 24)
  ; 551,602 -> 366,457
  (road city-loc-14 city-loc-15)
  (= (road-length city-loc-14 city-loc-15) 24)
  ; 453,848 -> 630,722
  (road city-loc-16 city-loc-3)
  (= (road-length city-loc-16 city-loc-3) 22)
  ; 630,722 -> 453,848
  (road city-loc-3 city-loc-16)
  (= (road-length city-loc-3 city-loc-16) 22)
  ; 453,848 -> 395,741
  (road city-loc-16 city-loc-8)
  (= (road-length city-loc-16 city-loc-8) 13)
  ; 395,741 -> 453,848
  (road city-loc-8 city-loc-16)
  (= (road-length city-loc-8 city-loc-16) 13)
  ; 614,343 -> 490,285
  (road city-loc-17 city-loc-1)
  (= (road-length city-loc-17 city-loc-1) 14)
  ; 490,285 -> 614,343
  (road city-loc-1 city-loc-17)
  (= (road-length city-loc-1 city-loc-17) 14)
  ; 614,343 -> 377,283
  (road city-loc-17 city-loc-5)
  (= (road-length city-loc-17 city-loc-5) 25)
  ; 377,283 -> 614,343
  (road city-loc-5 city-loc-17)
  (= (road-length city-loc-5 city-loc-17) 25)
  ; 936,210 -> 858,139
  (road city-loc-18 city-loc-10)
  (= (road-length city-loc-18 city-loc-10) 11)
  ; 858,139 -> 936,210
  (road city-loc-10 city-loc-18)
  (= (road-length city-loc-10 city-loc-18) 11)
  ; 193,424 -> 200,669
  (road city-loc-19 city-loc-2)
  (= (road-length city-loc-19 city-loc-2) 25)
  ; 200,669 -> 193,424
  (road city-loc-2 city-loc-19)
  (= (road-length city-loc-2 city-loc-19) 25)
  ; 193,424 -> 377,283
  (road city-loc-19 city-loc-5)
  (= (road-length city-loc-19 city-loc-5) 24)
  ; 377,283 -> 193,424
  (road city-loc-5 city-loc-19)
  (= (road-length city-loc-5 city-loc-19) 24)
  ; 193,424 -> 171,545
  (road city-loc-19 city-loc-6)
  (= (road-length city-loc-19 city-loc-6) 13)
  ; 171,545 -> 193,424
  (road city-loc-6 city-loc-19)
  (= (road-length city-loc-6 city-loc-19) 13)
  ; 193,424 -> 348,607
  (road city-loc-19 city-loc-7)
  (= (road-length city-loc-19 city-loc-7) 24)
  ; 348,607 -> 193,424
  (road city-loc-7 city-loc-19)
  (= (road-length city-loc-7 city-loc-19) 24)
  ; 193,424 -> 71,275
  (road city-loc-19 city-loc-9)
  (= (road-length city-loc-19 city-loc-9) 20)
  ; 71,275 -> 193,424
  (road city-loc-9 city-loc-19)
  (= (road-length city-loc-9 city-loc-19) 20)
  ; 193,424 -> 69,508
  (road city-loc-19 city-loc-11)
  (= (road-length city-loc-19 city-loc-11) 15)
  ; 69,508 -> 193,424
  (road city-loc-11 city-loc-19)
  (= (road-length city-loc-11 city-loc-19) 15)
  ; 193,424 -> 366,457
  (road city-loc-19 city-loc-15)
  (= (road-length city-loc-19 city-loc-15) 18)
  ; 366,457 -> 193,424
  (road city-loc-15 city-loc-19)
  (= (road-length city-loc-15 city-loc-19) 18)
  ; 480,435 -> 490,285
  (road city-loc-20 city-loc-1)
  (= (road-length city-loc-20 city-loc-1) 15)
  ; 490,285 -> 480,435
  (road city-loc-1 city-loc-20)
  (= (road-length city-loc-1 city-loc-20) 15)
  ; 480,435 -> 377,283
  (road city-loc-20 city-loc-5)
  (= (road-length city-loc-20 city-loc-5) 19)
  ; 377,283 -> 480,435
  (road city-loc-5 city-loc-20)
  (= (road-length city-loc-5 city-loc-20) 19)
  ; 480,435 -> 348,607
  (road city-loc-20 city-loc-7)
  (= (road-length city-loc-20 city-loc-7) 22)
  ; 348,607 -> 480,435
  (road city-loc-7 city-loc-20)
  (= (road-length city-loc-7 city-loc-20) 22)
  ; 480,435 -> 551,602
  (road city-loc-20 city-loc-14)
  (= (road-length city-loc-20 city-loc-14) 19)
  ; 551,602 -> 480,435
  (road city-loc-14 city-loc-20)
  (= (road-length city-loc-14 city-loc-20) 19)
  ; 480,435 -> 366,457
  (road city-loc-20 city-loc-15)
  (= (road-length city-loc-20 city-loc-15) 12)
  ; 366,457 -> 480,435
  (road city-loc-15 city-loc-20)
  (= (road-length city-loc-15 city-loc-20) 12)
  ; 480,435 -> 614,343
  (road city-loc-20 city-loc-17)
  (= (road-length city-loc-20 city-loc-17) 17)
  ; 614,343 -> 480,435
  (road city-loc-17 city-loc-20)
  (= (road-length city-loc-17 city-loc-20) 17)
  ; 918,341 -> 858,139
  (road city-loc-21 city-loc-10)
  (= (road-length city-loc-21 city-loc-10) 22)
  ; 858,139 -> 918,341
  (road city-loc-10 city-loc-21)
  (= (road-length city-loc-10 city-loc-21) 22)
  ; 918,341 -> 936,210
  (road city-loc-21 city-loc-18)
  (= (road-length city-loc-21 city-loc-18) 14)
  ; 936,210 -> 918,341
  (road city-loc-18 city-loc-21)
  (= (road-length city-loc-18 city-loc-21) 14)
  ; 651,235 -> 490,285
  (road city-loc-22 city-loc-1)
  (= (road-length city-loc-22 city-loc-1) 17)
  ; 490,285 -> 651,235
  (road city-loc-1 city-loc-22)
  (= (road-length city-loc-1 city-loc-22) 17)
  ; 651,235 -> 858,139
  (road city-loc-22 city-loc-10)
  (= (road-length city-loc-22 city-loc-10) 23)
  ; 858,139 -> 651,235
  (road city-loc-10 city-loc-22)
  (= (road-length city-loc-10 city-loc-22) 23)
  ; 651,235 -> 614,343
  (road city-loc-22 city-loc-17)
  (= (road-length city-loc-22 city-loc-17) 12)
  ; 614,343 -> 651,235
  (road city-loc-17 city-loc-22)
  (= (road-length city-loc-17 city-loc-22) 12)
  ; 560,901 -> 630,722
  (road city-loc-23 city-loc-3)
  (= (road-length city-loc-23 city-loc-3) 20)
  ; 630,722 -> 560,901
  (road city-loc-3 city-loc-23)
  (= (road-length city-loc-3 city-loc-23) 20)
  ; 560,901 -> 395,741
  (road city-loc-23 city-loc-8)
  (= (road-length city-loc-23 city-loc-8) 23)
  ; 395,741 -> 560,901
  (road city-loc-8 city-loc-23)
  (= (road-length city-loc-8 city-loc-23) 23)
  ; 560,901 -> 453,848
  (road city-loc-23 city-loc-16)
  (= (road-length city-loc-23 city-loc-16) 12)
  ; 453,848 -> 560,901
  (road city-loc-16 city-loc-23)
  (= (road-length city-loc-16 city-loc-23) 12)
  ; 362,940 -> 395,741
  (road city-loc-24 city-loc-8)
  (= (road-length city-loc-24 city-loc-8) 21)
  ; 395,741 -> 362,940
  (road city-loc-8 city-loc-24)
  (= (road-length city-loc-8 city-loc-24) 21)
  ; 362,940 -> 203,987
  (road city-loc-24 city-loc-12)
  (= (road-length city-loc-24 city-loc-12) 17)
  ; 203,987 -> 362,940
  (road city-loc-12 city-loc-24)
  (= (road-length city-loc-12 city-loc-24) 17)
  ; 362,940 -> 453,848
  (road city-loc-24 city-loc-16)
  (= (road-length city-loc-24 city-loc-16) 13)
  ; 453,848 -> 362,940
  (road city-loc-16 city-loc-24)
  (= (road-length city-loc-16 city-loc-24) 13)
  ; 362,940 -> 560,901
  (road city-loc-24 city-loc-23)
  (= (road-length city-loc-24 city-loc-23) 21)
  ; 560,901 -> 362,940
  (road city-loc-23 city-loc-24)
  (= (road-length city-loc-23 city-loc-24) 21)
  ; 941,734 -> 968,863
  (road city-loc-25 city-loc-13)
  (= (road-length city-loc-25 city-loc-13) 14)
  ; 968,863 -> 941,734
  (road city-loc-13 city-loc-25)
  (= (road-length city-loc-13 city-loc-25) 14)
  ; 653,507 -> 630,722
  (road city-loc-26 city-loc-3)
  (= (road-length city-loc-26 city-loc-3) 22)
  ; 630,722 -> 653,507
  (road city-loc-3 city-loc-26)
  (= (road-length city-loc-3 city-loc-26) 22)
  ; 653,507 -> 551,602
  (road city-loc-26 city-loc-14)
  (= (road-length city-loc-26 city-loc-14) 14)
  ; 551,602 -> 653,507
  (road city-loc-14 city-loc-26)
  (= (road-length city-loc-14 city-loc-26) 14)
  ; 653,507 -> 614,343
  (road city-loc-26 city-loc-17)
  (= (road-length city-loc-26 city-loc-17) 17)
  ; 614,343 -> 653,507
  (road city-loc-17 city-loc-26)
  (= (road-length city-loc-17 city-loc-26) 17)
  ; 653,507 -> 480,435
  (road city-loc-26 city-loc-20)
  (= (road-length city-loc-26 city-loc-20) 19)
  ; 480,435 -> 653,507
  (road city-loc-20 city-loc-26)
  (= (road-length city-loc-20 city-loc-26) 19)
  ; 820,551 -> 918,341
  (road city-loc-27 city-loc-21)
  (= (road-length city-loc-27 city-loc-21) 24)
  ; 918,341 -> 820,551
  (road city-loc-21 city-loc-27)
  (= (road-length city-loc-21 city-loc-27) 24)
  ; 820,551 -> 941,734
  (road city-loc-27 city-loc-25)
  (= (road-length city-loc-27 city-loc-25) 22)
  ; 941,734 -> 820,551
  (road city-loc-25 city-loc-27)
  (= (road-length city-loc-25 city-loc-27) 22)
  ; 820,551 -> 653,507
  (road city-loc-27 city-loc-26)
  (= (road-length city-loc-27 city-loc-26) 18)
  ; 653,507 -> 820,551
  (road city-loc-26 city-loc-27)
  (= (road-length city-loc-26 city-loc-27) 18)
  ; 731,24 -> 858,139
  (road city-loc-28 city-loc-10)
  (= (road-length city-loc-28 city-loc-10) 18)
  ; 858,139 -> 731,24
  (road city-loc-10 city-loc-28)
  (= (road-length city-loc-10 city-loc-28) 18)
  ; 731,24 -> 651,235
  (road city-loc-28 city-loc-22)
  (= (road-length city-loc-28 city-loc-22) 23)
  ; 651,235 -> 731,24
  (road city-loc-22 city-loc-28)
  (= (road-length city-loc-22 city-loc-28) 23)
  ; 452,192 -> 490,285
  (road city-loc-29 city-loc-1)
  (= (road-length city-loc-29 city-loc-1) 10)
  ; 490,285 -> 452,192
  (road city-loc-1 city-loc-29)
  (= (road-length city-loc-1 city-loc-29) 10)
  ; 452,192 -> 377,283
  (road city-loc-29 city-loc-5)
  (= (road-length city-loc-29 city-loc-5) 12)
  ; 377,283 -> 452,192
  (road city-loc-5 city-loc-29)
  (= (road-length city-loc-5 city-loc-29) 12)
  ; 452,192 -> 614,343
  (road city-loc-29 city-loc-17)
  (= (road-length city-loc-29 city-loc-17) 23)
  ; 614,343 -> 452,192
  (road city-loc-17 city-loc-29)
  (= (road-length city-loc-17 city-loc-29) 23)
  ; 452,192 -> 480,435
  (road city-loc-29 city-loc-20)
  (= (road-length city-loc-29 city-loc-20) 25)
  ; 480,435 -> 452,192
  (road city-loc-20 city-loc-29)
  (= (road-length city-loc-20 city-loc-29) 25)
  ; 452,192 -> 651,235
  (road city-loc-29 city-loc-22)
  (= (road-length city-loc-29 city-loc-22) 21)
  ; 651,235 -> 452,192
  (road city-loc-22 city-loc-29)
  (= (road-length city-loc-22 city-loc-29) 21)
  ; 205,275 -> 377,283
  (road city-loc-30 city-loc-5)
  (= (road-length city-loc-30 city-loc-5) 18)
  ; 377,283 -> 205,275
  (road city-loc-5 city-loc-30)
  (= (road-length city-loc-5 city-loc-30) 18)
  ; 205,275 -> 71,275
  (road city-loc-30 city-loc-9)
  (= (road-length city-loc-30 city-loc-9) 14)
  ; 71,275 -> 205,275
  (road city-loc-9 city-loc-30)
  (= (road-length city-loc-9 city-loc-30) 14)
  ; 205,275 -> 366,457
  (road city-loc-30 city-loc-15)
  (= (road-length city-loc-30 city-loc-15) 25)
  ; 366,457 -> 205,275
  (road city-loc-15 city-loc-30)
  (= (road-length city-loc-15 city-loc-30) 25)
  ; 205,275 -> 193,424
  (road city-loc-30 city-loc-19)
  (= (road-length city-loc-30 city-loc-19) 15)
  ; 193,424 -> 205,275
  (road city-loc-19 city-loc-30)
  (= (road-length city-loc-19 city-loc-30) 15)
  (at package-1 city-loc-4)
  (at package-2 city-loc-23)
  (at package-3 city-loc-9)
  (at package-4 city-loc-7)
  (at package-5 city-loc-6)
  (at package-6 city-loc-13)
  (at package-7 city-loc-1)
  (at package-8 city-loc-16)
  (at package-9 city-loc-11)
  (at package-10 city-loc-12)
  (at package-11 city-loc-19)
  (at truck-1 city-loc-8)
  (capacity truck-1 capacity-2)
  (at truck-2 city-loc-6)
  (capacity truck-2 capacity-4)
  (at truck-3 city-loc-24)
  (capacity truck-3 capacity-4)
 )
 (:goal (and
  (at package-1 city-loc-29)
  (at package-2 city-loc-20)
  (at package-3 city-loc-28)
  (at package-4 city-loc-28)
  (at package-5 city-loc-5)
  (at package-6 city-loc-5)
  (at package-7 city-loc-30)
  (at package-8 city-loc-29)
  (at package-9 city-loc-4)
  (at package-10 city-loc-28)
  (at package-11 city-loc-27)
 ))
 (:metric minimize (total-cost))
)
