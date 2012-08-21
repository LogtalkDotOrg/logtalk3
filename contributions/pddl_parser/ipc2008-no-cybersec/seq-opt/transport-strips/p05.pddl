; Transport city-sequential-15nodes-1000size-3degree-100mindistance-2trucks-6packages-2008seed

(define (problem transport-city-sequential-15nodes-1000size-3degree-100mindistance-2trucks-6packages-2008seed)
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
  ; 347,149 -> 257,5
  (road city-loc-4 city-loc-1)
  (= (road-length city-loc-4 city-loc-1) 17)
  ; 257,5 -> 347,149
  (road city-loc-1 city-loc-4)
  (= (road-length city-loc-1 city-loc-4) 17)
  ; 347,149 -> 245,346
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 23)
  ; 245,346 -> 347,149
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 23)
  ; 336,475 -> 245,346
  (road city-loc-5 city-loc-2)
  (= (road-length city-loc-5 city-loc-2) 16)
  ; 245,346 -> 336,475
  (road city-loc-2 city-loc-5)
  (= (road-length city-loc-2 city-loc-5) 16)
  ; 336,475 -> 559,565
  (road city-loc-5 city-loc-3)
  (= (road-length city-loc-5 city-loc-3) 24)
  ; 559,565 -> 336,475
  (road city-loc-3 city-loc-5)
  (= (road-length city-loc-3 city-loc-5) 24)
  ; 170,709 -> 336,475
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 29)
  ; 336,475 -> 170,709
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 29)
  ; 521,375 -> 245,346
  (road city-loc-7 city-loc-2)
  (= (road-length city-loc-7 city-loc-2) 28)
  ; 245,346 -> 521,375
  (road city-loc-2 city-loc-7)
  (= (road-length city-loc-2 city-loc-7) 28)
  ; 521,375 -> 559,565
  (road city-loc-7 city-loc-3)
  (= (road-length city-loc-7 city-loc-3) 20)
  ; 559,565 -> 521,375
  (road city-loc-3 city-loc-7)
  (= (road-length city-loc-3 city-loc-7) 20)
  ; 521,375 -> 347,149
  (road city-loc-7 city-loc-4)
  (= (road-length city-loc-7 city-loc-4) 29)
  ; 347,149 -> 521,375
  (road city-loc-4 city-loc-7)
  (= (road-length city-loc-4 city-loc-7) 29)
  ; 521,375 -> 336,475
  (road city-loc-7 city-loc-5)
  (= (road-length city-loc-7 city-loc-5) 21)
  ; 336,475 -> 521,375
  (road city-loc-5 city-loc-7)
  (= (road-length city-loc-5 city-loc-7) 21)
  ; 720,241 -> 521,375
  (road city-loc-9 city-loc-7)
  (= (road-length city-loc-9 city-loc-7) 24)
  ; 521,375 -> 720,241
  (road city-loc-7 city-loc-9)
  (= (road-length city-loc-7 city-loc-9) 24)
  ; 720,241 -> 701,0
  (road city-loc-9 city-loc-8)
  (= (road-length city-loc-9 city-loc-8) 25)
  ; 701,0 -> 720,241
  (road city-loc-8 city-loc-9)
  (= (road-length city-loc-8 city-loc-9) 25)
  ; 630,722 -> 559,565
  (road city-loc-10 city-loc-3)
  (= (road-length city-loc-10 city-loc-3) 18)
  ; 559,565 -> 630,722
  (road city-loc-3 city-loc-10)
  (= (road-length city-loc-3 city-loc-10) 18)
  ; 120,854 -> 170,709
  (road city-loc-11 city-loc-6)
  (= (road-length city-loc-11 city-loc-6) 16)
  ; 170,709 -> 120,854
  (road city-loc-6 city-loc-11)
  (= (road-length city-loc-6 city-loc-11) 16)
  ; 377,283 -> 245,346
  (road city-loc-12 city-loc-2)
  (= (road-length city-loc-12 city-loc-2) 15)
  ; 245,346 -> 377,283
  (road city-loc-2 city-loc-12)
  (= (road-length city-loc-2 city-loc-12) 15)
  ; 377,283 -> 347,149
  (road city-loc-12 city-loc-4)
  (= (road-length city-loc-12 city-loc-4) 14)
  ; 347,149 -> 377,283
  (road city-loc-4 city-loc-12)
  (= (road-length city-loc-4 city-loc-12) 14)
  ; 377,283 -> 336,475
  (road city-loc-12 city-loc-5)
  (= (road-length city-loc-12 city-loc-5) 20)
  ; 336,475 -> 377,283
  (road city-loc-5 city-loc-12)
  (= (road-length city-loc-5 city-loc-12) 20)
  ; 377,283 -> 521,375
  (road city-loc-12 city-loc-7)
  (= (road-length city-loc-12 city-loc-7) 18)
  ; 521,375 -> 377,283
  (road city-loc-7 city-loc-12)
  (= (road-length city-loc-7 city-loc-12) 18)
  ; 171,545 -> 245,346
  (road city-loc-13 city-loc-2)
  (= (road-length city-loc-13 city-loc-2) 22)
  ; 245,346 -> 171,545
  (road city-loc-2 city-loc-13)
  (= (road-length city-loc-2 city-loc-13) 22)
  ; 171,545 -> 336,475
  (road city-loc-13 city-loc-5)
  (= (road-length city-loc-13 city-loc-5) 18)
  ; 336,475 -> 171,545
  (road city-loc-5 city-loc-13)
  (= (road-length city-loc-5 city-loc-13) 18)
  ; 171,545 -> 170,709
  (road city-loc-13 city-loc-6)
  (= (road-length city-loc-13 city-loc-6) 17)
  ; 170,709 -> 171,545
  (road city-loc-6 city-loc-13)
  (= (road-length city-loc-6 city-loc-13) 17)
  ; 348,607 -> 245,346
  (road city-loc-14 city-loc-2)
  (= (road-length city-loc-14 city-loc-2) 29)
  ; 245,346 -> 348,607
  (road city-loc-2 city-loc-14)
  (= (road-length city-loc-2 city-loc-14) 29)
  ; 348,607 -> 559,565
  (road city-loc-14 city-loc-3)
  (= (road-length city-loc-14 city-loc-3) 22)
  ; 559,565 -> 348,607
  (road city-loc-3 city-loc-14)
  (= (road-length city-loc-3 city-loc-14) 22)
  ; 348,607 -> 336,475
  (road city-loc-14 city-loc-5)
  (= (road-length city-loc-14 city-loc-5) 14)
  ; 336,475 -> 348,607
  (road city-loc-5 city-loc-14)
  (= (road-length city-loc-5 city-loc-14) 14)
  ; 348,607 -> 170,709
  (road city-loc-14 city-loc-6)
  (= (road-length city-loc-14 city-loc-6) 21)
  ; 170,709 -> 348,607
  (road city-loc-6 city-loc-14)
  (= (road-length city-loc-6 city-loc-14) 21)
  ; 348,607 -> 521,375
  (road city-loc-14 city-loc-7)
  (= (road-length city-loc-14 city-loc-7) 29)
  ; 521,375 -> 348,607
  (road city-loc-7 city-loc-14)
  (= (road-length city-loc-7 city-loc-14) 29)
  ; 348,607 -> 171,545
  (road city-loc-14 city-loc-13)
  (= (road-length city-loc-14 city-loc-13) 19)
  ; 171,545 -> 348,607
  (road city-loc-13 city-loc-14)
  (= (road-length city-loc-13 city-loc-14) 19)
  ; 395,741 -> 559,565
  (road city-loc-15 city-loc-3)
  (= (road-length city-loc-15 city-loc-3) 25)
  ; 559,565 -> 395,741
  (road city-loc-3 city-loc-15)
  (= (road-length city-loc-3 city-loc-15) 25)
  ; 395,741 -> 336,475
  (road city-loc-15 city-loc-5)
  (= (road-length city-loc-15 city-loc-5) 28)
  ; 336,475 -> 395,741
  (road city-loc-5 city-loc-15)
  (= (road-length city-loc-5 city-loc-15) 28)
  ; 395,741 -> 170,709
  (road city-loc-15 city-loc-6)
  (= (road-length city-loc-15 city-loc-6) 23)
  ; 170,709 -> 395,741
  (road city-loc-6 city-loc-15)
  (= (road-length city-loc-6 city-loc-15) 23)
  ; 395,741 -> 630,722
  (road city-loc-15 city-loc-10)
  (= (road-length city-loc-15 city-loc-10) 24)
  ; 630,722 -> 395,741
  (road city-loc-10 city-loc-15)
  (= (road-length city-loc-10 city-loc-15) 24)
  ; 395,741 -> 120,854
  (road city-loc-15 city-loc-11)
  (= (road-length city-loc-15 city-loc-11) 30)
  ; 120,854 -> 395,741
  (road city-loc-11 city-loc-15)
  (= (road-length city-loc-11 city-loc-15) 30)
  ; 395,741 -> 171,545
  (road city-loc-15 city-loc-13)
  (= (road-length city-loc-15 city-loc-13) 30)
  ; 171,545 -> 395,741
  (road city-loc-13 city-loc-15)
  (= (road-length city-loc-13 city-loc-15) 30)
  ; 395,741 -> 348,607
  (road city-loc-15 city-loc-14)
  (= (road-length city-loc-15 city-loc-14) 15)
  ; 348,607 -> 395,741
  (road city-loc-14 city-loc-15)
  (= (road-length city-loc-14 city-loc-15) 15)
  (at package-1 city-loc-10)
  (at package-2 city-loc-11)
  (at package-3 city-loc-5)
  (at package-4 city-loc-9)
  (at package-5 city-loc-6)
  (at package-6 city-loc-4)
  (at truck-1 city-loc-12)
  (capacity truck-1 capacity-3)
  (at truck-2 city-loc-5)
  (capacity truck-2 capacity-2)
 )
 (:goal (and
  (at package-1 city-loc-13)
  (at package-2 city-loc-3)
  (at package-3 city-loc-2)
  (at package-4 city-loc-8)
  (at package-5 city-loc-4)
  (at package-6 city-loc-15)
 ))
 (:metric minimize (total-cost))
)
