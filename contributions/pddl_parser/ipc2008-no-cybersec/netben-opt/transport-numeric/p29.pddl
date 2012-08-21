; Transport two-cities-netbenefit-11nodes-700size-3degree-70mindistance-3trucks-6packages-6024seed

(define (problem transport-two-cities-netbenefit-11nodes-700size-3degree-70mindistance-3trucks-6packages-6024seed)
 (:domain transport)
 (:objects
  city-1-loc-1 - location
  city-2-loc-1 - location
  city-1-loc-2 - location
  city-2-loc-2 - location
  city-1-loc-3 - location
  city-2-loc-3 - location
  city-1-loc-4 - location
  city-2-loc-4 - location
  city-1-loc-5 - location
  city-2-loc-5 - location
  city-1-loc-6 - location
  city-2-loc-6 - location
  city-1-loc-7 - location
  city-2-loc-7 - location
  city-1-loc-8 - location
  city-2-loc-8 - location
  city-1-loc-9 - location
  city-2-loc-9 - location
  city-1-loc-10 - location
  city-2-loc-10 - location
  city-1-loc-11 - location
  city-2-loc-11 - location
  truck-1 - vehicle
  truck-2 - vehicle
  truck-3 - vehicle
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
  ; 390,532 -> 459,347
  (road city-1-loc-2 city-1-loc-1)
  (= (road-length city-1-loc-2 city-1-loc-1) 20)
  (= (fuel-demand city-1-loc-2 city-1-loc-1) 40)
  ; 459,347 -> 390,532
  (road city-1-loc-1 city-1-loc-2)
  (= (road-length city-1-loc-1 city-1-loc-2) 20)
  (= (fuel-demand city-1-loc-1 city-1-loc-2) 40)
  ; 340,297 -> 459,347
  (road city-1-loc-5 city-1-loc-1)
  (= (road-length city-1-loc-5 city-1-loc-1) 13)
  (= (fuel-demand city-1-loc-5 city-1-loc-1) 26)
  ; 459,347 -> 340,297
  (road city-1-loc-1 city-1-loc-5)
  (= (road-length city-1-loc-1 city-1-loc-5) 13)
  (= (fuel-demand city-1-loc-1 city-1-loc-5) 26)
  ; 340,297 -> 390,532
  (road city-1-loc-5 city-1-loc-2)
  (= (road-length city-1-loc-5 city-1-loc-2) 24)
  (= (fuel-demand city-1-loc-5 city-1-loc-2) 48)
  ; 390,532 -> 340,297
  (road city-1-loc-2 city-1-loc-5)
  (= (road-length city-1-loc-2 city-1-loc-5) 24)
  (= (fuel-demand city-1-loc-2 city-1-loc-5) 48)
  ; 340,297 -> 263,105
  (road city-1-loc-5 city-1-loc-3)
  (= (road-length city-1-loc-5 city-1-loc-3) 21)
  (= (fuel-demand city-1-loc-5 city-1-loc-3) 42)
  ; 263,105 -> 340,297
  (road city-1-loc-3 city-1-loc-5)
  (= (road-length city-1-loc-3 city-1-loc-5) 21)
  (= (fuel-demand city-1-loc-3 city-1-loc-5) 42)
  ; 279,346 -> 459,347
  (road city-1-loc-6 city-1-loc-1)
  (= (road-length city-1-loc-6 city-1-loc-1) 18)
  (= (fuel-demand city-1-loc-6 city-1-loc-1) 36)
  ; 459,347 -> 279,346
  (road city-1-loc-1 city-1-loc-6)
  (= (road-length city-1-loc-1 city-1-loc-6) 18)
  (= (fuel-demand city-1-loc-1 city-1-loc-6) 36)
  ; 279,346 -> 390,532
  (road city-1-loc-6 city-1-loc-2)
  (= (road-length city-1-loc-6 city-1-loc-2) 22)
  (= (fuel-demand city-1-loc-6 city-1-loc-2) 44)
  ; 390,532 -> 279,346
  (road city-1-loc-2 city-1-loc-6)
  (= (road-length city-1-loc-2 city-1-loc-6) 22)
  (= (fuel-demand city-1-loc-2 city-1-loc-6) 44)
  ; 279,346 -> 263,105
  (road city-1-loc-6 city-1-loc-3)
  (= (road-length city-1-loc-6 city-1-loc-3) 25)
  (= (fuel-demand city-1-loc-6 city-1-loc-3) 49)
  ; 263,105 -> 279,346
  (road city-1-loc-3 city-1-loc-6)
  (= (road-length city-1-loc-3 city-1-loc-6) 25)
  (= (fuel-demand city-1-loc-3 city-1-loc-6) 49)
  ; 279,346 -> 127,449
  (road city-1-loc-6 city-1-loc-4)
  (= (road-length city-1-loc-6 city-1-loc-4) 19)
  (= (fuel-demand city-1-loc-6 city-1-loc-4) 37)
  ; 127,449 -> 279,346
  (road city-1-loc-4 city-1-loc-6)
  (= (road-length city-1-loc-4 city-1-loc-6) 19)
  (= (fuel-demand city-1-loc-4 city-1-loc-6) 37)
  ; 279,346 -> 340,297
  (road city-1-loc-6 city-1-loc-5)
  (= (road-length city-1-loc-6 city-1-loc-5) 8)
  (= (fuel-demand city-1-loc-6 city-1-loc-5) 16)
  ; 340,297 -> 279,346
  (road city-1-loc-5 city-1-loc-6)
  (= (road-length city-1-loc-5 city-1-loc-6) 8)
  (= (fuel-demand city-1-loc-5 city-1-loc-6) 16)
  ; 297,501 -> 459,347
  (road city-1-loc-7 city-1-loc-1)
  (= (road-length city-1-loc-7 city-1-loc-1) 23)
  (= (fuel-demand city-1-loc-7 city-1-loc-1) 45)
  ; 459,347 -> 297,501
  (road city-1-loc-1 city-1-loc-7)
  (= (road-length city-1-loc-1 city-1-loc-7) 23)
  (= (fuel-demand city-1-loc-1 city-1-loc-7) 45)
  ; 297,501 -> 390,532
  (road city-1-loc-7 city-1-loc-2)
  (= (road-length city-1-loc-7 city-1-loc-2) 10)
  (= (fuel-demand city-1-loc-7 city-1-loc-2) 20)
  ; 390,532 -> 297,501
  (road city-1-loc-2 city-1-loc-7)
  (= (road-length city-1-loc-2 city-1-loc-7) 10)
  (= (fuel-demand city-1-loc-2 city-1-loc-7) 20)
  ; 297,501 -> 127,449
  (road city-1-loc-7 city-1-loc-4)
  (= (road-length city-1-loc-7 city-1-loc-4) 18)
  (= (fuel-demand city-1-loc-7 city-1-loc-4) 36)
  ; 127,449 -> 297,501
  (road city-1-loc-4 city-1-loc-7)
  (= (road-length city-1-loc-4 city-1-loc-7) 18)
  (= (fuel-demand city-1-loc-4 city-1-loc-7) 36)
  ; 297,501 -> 340,297
  (road city-1-loc-7 city-1-loc-5)
  (= (road-length city-1-loc-7 city-1-loc-5) 21)
  (= (fuel-demand city-1-loc-7 city-1-loc-5) 42)
  ; 340,297 -> 297,501
  (road city-1-loc-5 city-1-loc-7)
  (= (road-length city-1-loc-5 city-1-loc-7) 21)
  (= (fuel-demand city-1-loc-5 city-1-loc-7) 42)
  ; 297,501 -> 279,346
  (road city-1-loc-7 city-1-loc-6)
  (= (road-length city-1-loc-7 city-1-loc-6) 16)
  (= (fuel-demand city-1-loc-7 city-1-loc-6) 32)
  ; 279,346 -> 297,501
  (road city-1-loc-6 city-1-loc-7)
  (= (road-length city-1-loc-6 city-1-loc-7) 16)
  (= (fuel-demand city-1-loc-6 city-1-loc-7) 32)
  ; 390,452 -> 459,347
  (road city-1-loc-8 city-1-loc-1)
  (= (road-length city-1-loc-8 city-1-loc-1) 13)
  (= (fuel-demand city-1-loc-8 city-1-loc-1) 26)
  ; 459,347 -> 390,452
  (road city-1-loc-1 city-1-loc-8)
  (= (road-length city-1-loc-1 city-1-loc-8) 13)
  (= (fuel-demand city-1-loc-1 city-1-loc-8) 26)
  ; 390,452 -> 390,532
  (road city-1-loc-8 city-1-loc-2)
  (= (road-length city-1-loc-8 city-1-loc-2) 8)
  (= (fuel-demand city-1-loc-8 city-1-loc-2) 16)
  ; 390,532 -> 390,452
  (road city-1-loc-2 city-1-loc-8)
  (= (road-length city-1-loc-2 city-1-loc-8) 8)
  (= (fuel-demand city-1-loc-2 city-1-loc-8) 16)
  ; 390,452 -> 340,297
  (road city-1-loc-8 city-1-loc-5)
  (= (road-length city-1-loc-8 city-1-loc-5) 17)
  (= (fuel-demand city-1-loc-8 city-1-loc-5) 33)
  ; 340,297 -> 390,452
  (road city-1-loc-5 city-1-loc-8)
  (= (road-length city-1-loc-5 city-1-loc-8) 17)
  (= (fuel-demand city-1-loc-5 city-1-loc-8) 33)
  ; 390,452 -> 279,346
  (road city-1-loc-8 city-1-loc-6)
  (= (road-length city-1-loc-8 city-1-loc-6) 16)
  (= (fuel-demand city-1-loc-8 city-1-loc-6) 31)
  ; 279,346 -> 390,452
  (road city-1-loc-6 city-1-loc-8)
  (= (road-length city-1-loc-6 city-1-loc-8) 16)
  (= (fuel-demand city-1-loc-6 city-1-loc-8) 31)
  ; 390,452 -> 297,501
  (road city-1-loc-8 city-1-loc-7)
  (= (road-length city-1-loc-8 city-1-loc-7) 11)
  (= (fuel-demand city-1-loc-8 city-1-loc-7) 21)
  ; 297,501 -> 390,452
  (road city-1-loc-7 city-1-loc-8)
  (= (road-length city-1-loc-7 city-1-loc-8) 11)
  (= (fuel-demand city-1-loc-7 city-1-loc-8) 21)
  ; 116,330 -> 127,449
  (road city-1-loc-9 city-1-loc-4)
  (= (road-length city-1-loc-9 city-1-loc-4) 12)
  (= (fuel-demand city-1-loc-9 city-1-loc-4) 24)
  ; 127,449 -> 116,330
  (road city-1-loc-4 city-1-loc-9)
  (= (road-length city-1-loc-4 city-1-loc-9) 12)
  (= (fuel-demand city-1-loc-4 city-1-loc-9) 24)
  ; 116,330 -> 340,297
  (road city-1-loc-9 city-1-loc-5)
  (= (road-length city-1-loc-9 city-1-loc-5) 23)
  (= (fuel-demand city-1-loc-9 city-1-loc-5) 46)
  ; 340,297 -> 116,330
  (road city-1-loc-5 city-1-loc-9)
  (= (road-length city-1-loc-5 city-1-loc-9) 23)
  (= (fuel-demand city-1-loc-5 city-1-loc-9) 46)
  ; 116,330 -> 279,346
  (road city-1-loc-9 city-1-loc-6)
  (= (road-length city-1-loc-9 city-1-loc-6) 17)
  (= (fuel-demand city-1-loc-9 city-1-loc-6) 33)
  ; 279,346 -> 116,330
  (road city-1-loc-6 city-1-loc-9)
  (= (road-length city-1-loc-6 city-1-loc-9) 17)
  (= (fuel-demand city-1-loc-6 city-1-loc-9) 33)
  ; 172,692 -> 127,449
  (road city-1-loc-10 city-1-loc-4)
  (= (road-length city-1-loc-10 city-1-loc-4) 25)
  (= (fuel-demand city-1-loc-10 city-1-loc-4) 50)
  ; 127,449 -> 172,692
  (road city-1-loc-4 city-1-loc-10)
  (= (road-length city-1-loc-4 city-1-loc-10) 25)
  (= (fuel-demand city-1-loc-4 city-1-loc-10) 50)
  ; 172,692 -> 297,501
  (road city-1-loc-10 city-1-loc-7)
  (= (road-length city-1-loc-10 city-1-loc-7) 23)
  (= (fuel-demand city-1-loc-10 city-1-loc-7) 46)
  ; 297,501 -> 172,692
  (road city-1-loc-7 city-1-loc-10)
  (= (road-length city-1-loc-7 city-1-loc-10) 23)
  (= (fuel-demand city-1-loc-7 city-1-loc-10) 46)
  ; 32,519 -> 127,449
  (road city-1-loc-11 city-1-loc-4)
  (= (road-length city-1-loc-11 city-1-loc-4) 12)
  (= (fuel-demand city-1-loc-11 city-1-loc-4) 24)
  ; 127,449 -> 32,519
  (road city-1-loc-4 city-1-loc-11)
  (= (road-length city-1-loc-4 city-1-loc-11) 12)
  (= (fuel-demand city-1-loc-4 city-1-loc-11) 24)
  ; 32,519 -> 116,330
  (road city-1-loc-11 city-1-loc-9)
  (= (road-length city-1-loc-11 city-1-loc-9) 21)
  (= (fuel-demand city-1-loc-11 city-1-loc-9) 42)
  ; 116,330 -> 32,519
  (road city-1-loc-9 city-1-loc-11)
  (= (road-length city-1-loc-9 city-1-loc-11) 21)
  (= (fuel-demand city-1-loc-9 city-1-loc-11) 42)
  ; 32,519 -> 172,692
  (road city-1-loc-11 city-1-loc-10)
  (= (road-length city-1-loc-11 city-1-loc-10) 23)
  (= (fuel-demand city-1-loc-11 city-1-loc-10) 45)
  ; 172,692 -> 32,519
  (road city-1-loc-10 city-1-loc-11)
  (= (road-length city-1-loc-10 city-1-loc-11) 23)
  (= (fuel-demand city-1-loc-10 city-1-loc-11) 45)
  ; 1617,408 -> 1483,455
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 15)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 29)
  ; 1483,455 -> 1617,408
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 15)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 29)
  ; 1580,538 -> 1483,455
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 13)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 26)
  ; 1483,455 -> 1580,538
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 13)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 26)
  ; 1580,538 -> 1617,408
  (road city-2-loc-3 city-2-loc-2)
  (= (road-length city-2-loc-3 city-2-loc-2) 14)
  (= (fuel-demand city-2-loc-3 city-2-loc-2) 27)
  ; 1617,408 -> 1580,538
  (road city-2-loc-2 city-2-loc-3)
  (= (road-length city-2-loc-2 city-2-loc-3) 14)
  (= (fuel-demand city-2-loc-2 city-2-loc-3) 27)
  ; 1956,431 -> 1864,299
  (road city-2-loc-6 city-2-loc-5)
  (= (road-length city-2-loc-6 city-2-loc-5) 17)
  (= (fuel-demand city-2-loc-6 city-2-loc-5) 33)
  ; 1864,299 -> 1956,431
  (road city-2-loc-5 city-2-loc-6)
  (= (road-length city-2-loc-5 city-2-loc-6) 17)
  (= (fuel-demand city-2-loc-5 city-2-loc-6) 33)
  ; 1635,278 -> 1483,455
  (road city-2-loc-7 city-2-loc-1)
  (= (road-length city-2-loc-7 city-2-loc-1) 24)
  (= (fuel-demand city-2-loc-7 city-2-loc-1) 47)
  ; 1483,455 -> 1635,278
  (road city-2-loc-1 city-2-loc-7)
  (= (road-length city-2-loc-1 city-2-loc-7) 24)
  (= (fuel-demand city-2-loc-1 city-2-loc-7) 47)
  ; 1635,278 -> 1617,408
  (road city-2-loc-7 city-2-loc-2)
  (= (road-length city-2-loc-7 city-2-loc-2) 14)
  (= (fuel-demand city-2-loc-7 city-2-loc-2) 27)
  ; 1617,408 -> 1635,278
  (road city-2-loc-2 city-2-loc-7)
  (= (road-length city-2-loc-2 city-2-loc-7) 14)
  (= (fuel-demand city-2-loc-2 city-2-loc-7) 27)
  ; 1635,278 -> 1864,299
  (road city-2-loc-7 city-2-loc-5)
  (= (road-length city-2-loc-7 city-2-loc-5) 23)
  (= (fuel-demand city-2-loc-7 city-2-loc-5) 46)
  ; 1864,299 -> 1635,278
  (road city-2-loc-5 city-2-loc-7)
  (= (road-length city-2-loc-5 city-2-loc-7) 23)
  (= (fuel-demand city-2-loc-5 city-2-loc-7) 46)
  ; 1620,113 -> 1832,35
  (road city-2-loc-8 city-2-loc-4)
  (= (road-length city-2-loc-8 city-2-loc-4) 23)
  (= (fuel-demand city-2-loc-8 city-2-loc-4) 46)
  ; 1832,35 -> 1620,113
  (road city-2-loc-4 city-2-loc-8)
  (= (road-length city-2-loc-4 city-2-loc-8) 23)
  (= (fuel-demand city-2-loc-4 city-2-loc-8) 46)
  ; 1620,113 -> 1635,278
  (road city-2-loc-8 city-2-loc-7)
  (= (road-length city-2-loc-8 city-2-loc-7) 17)
  (= (fuel-demand city-2-loc-8 city-2-loc-7) 34)
  ; 1635,278 -> 1620,113
  (road city-2-loc-7 city-2-loc-8)
  (= (road-length city-2-loc-7 city-2-loc-8) 17)
  (= (fuel-demand city-2-loc-7 city-2-loc-8) 34)
  ; 1988,6 -> 1832,35
  (road city-2-loc-9 city-2-loc-4)
  (= (road-length city-2-loc-9 city-2-loc-4) 16)
  (= (fuel-demand city-2-loc-9 city-2-loc-4) 32)
  ; 1832,35 -> 1988,6
  (road city-2-loc-4 city-2-loc-9)
  (= (road-length city-2-loc-4 city-2-loc-9) 16)
  (= (fuel-demand city-2-loc-4 city-2-loc-9) 32)
  ; 1717,182 -> 1617,408
  (road city-2-loc-10 city-2-loc-2)
  (= (road-length city-2-loc-10 city-2-loc-2) 25)
  (= (fuel-demand city-2-loc-10 city-2-loc-2) 50)
  ; 1617,408 -> 1717,182
  (road city-2-loc-2 city-2-loc-10)
  (= (road-length city-2-loc-2 city-2-loc-10) 25)
  (= (fuel-demand city-2-loc-2 city-2-loc-10) 50)
  ; 1717,182 -> 1832,35
  (road city-2-loc-10 city-2-loc-4)
  (= (road-length city-2-loc-10 city-2-loc-4) 19)
  (= (fuel-demand city-2-loc-10 city-2-loc-4) 38)
  ; 1832,35 -> 1717,182
  (road city-2-loc-4 city-2-loc-10)
  (= (road-length city-2-loc-4 city-2-loc-10) 19)
  (= (fuel-demand city-2-loc-4 city-2-loc-10) 38)
  ; 1717,182 -> 1864,299
  (road city-2-loc-10 city-2-loc-5)
  (= (road-length city-2-loc-10 city-2-loc-5) 19)
  (= (fuel-demand city-2-loc-10 city-2-loc-5) 38)
  ; 1864,299 -> 1717,182
  (road city-2-loc-5 city-2-loc-10)
  (= (road-length city-2-loc-5 city-2-loc-10) 19)
  (= (fuel-demand city-2-loc-5 city-2-loc-10) 38)
  ; 1717,182 -> 1635,278
  (road city-2-loc-10 city-2-loc-7)
  (= (road-length city-2-loc-10 city-2-loc-7) 13)
  (= (fuel-demand city-2-loc-10 city-2-loc-7) 26)
  ; 1635,278 -> 1717,182
  (road city-2-loc-7 city-2-loc-10)
  (= (road-length city-2-loc-7 city-2-loc-10) 13)
  (= (fuel-demand city-2-loc-7 city-2-loc-10) 26)
  ; 1717,182 -> 1620,113
  (road city-2-loc-10 city-2-loc-8)
  (= (road-length city-2-loc-10 city-2-loc-8) 12)
  (= (fuel-demand city-2-loc-10 city-2-loc-8) 24)
  ; 1620,113 -> 1717,182
  (road city-2-loc-8 city-2-loc-10)
  (= (road-length city-2-loc-8 city-2-loc-10) 12)
  (= (fuel-demand city-2-loc-8 city-2-loc-10) 24)
  ; 2053,391 -> 1864,299
  (road city-2-loc-11 city-2-loc-5)
  (= (road-length city-2-loc-11 city-2-loc-5) 21)
  (= (fuel-demand city-2-loc-11 city-2-loc-5) 42)
  ; 1864,299 -> 2053,391
  (road city-2-loc-5 city-2-loc-11)
  (= (road-length city-2-loc-5 city-2-loc-11) 21)
  (= (fuel-demand city-2-loc-5 city-2-loc-11) 42)
  ; 2053,391 -> 1956,431
  (road city-2-loc-11 city-2-loc-6)
  (= (road-length city-2-loc-11 city-2-loc-6) 11)
  (= (fuel-demand city-2-loc-11 city-2-loc-6) 21)
  ; 1956,431 -> 2053,391
  (road city-2-loc-6 city-2-loc-11)
  (= (road-length city-2-loc-6 city-2-loc-11) 11)
  (= (fuel-demand city-2-loc-6 city-2-loc-11) 21)
  ; 459,347 <-> 1483,455
  (road city-1-loc-1 city-2-loc-1)
  (= (road-length city-1-loc-1 city-2-loc-1) 103)
  (= (fuel-demand city-1-loc-1 city-2-loc-1) 52)
  (road city-2-loc-1 city-1-loc-1)
  (= (road-length city-2-loc-1 city-1-loc-1) 103)
  (= (fuel-demand city-2-loc-1 city-1-loc-1) 52)
  (has-petrol-station city-1-loc-1)
  (has-petrol-station city-2-loc-1)
  (at package-1 city-1-loc-8)
  (at package-2 city-1-loc-7)
  (at package-3 city-1-loc-1)
  (at package-4 city-1-loc-9)
  (at package-5 city-1-loc-8)
  (at package-6 city-1-loc-9)
  (at truck-1 city-2-loc-5)
  (= (fuel-left truck-1) 772)
  (= (fuel-max truck-1) 772)
  (capacity truck-1 capacity-4)
  (at truck-2 city-2-loc-4)
  (= (fuel-left truck-2) 772)
  (= (fuel-max truck-2) 772)
  (capacity truck-2 capacity-3)
  (at truck-3 city-2-loc-9)
  (= (fuel-left truck-3) 772)
  (= (fuel-max truck-3) 772)
  (capacity truck-3 capacity-4)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-11))
  (preference delivery-2 (at package-2 city-2-loc-11))
  (preference delivery-3 (at package-3 city-2-loc-2))
  (preference delivery-4 (at package-4 city-2-loc-4))
  (preference delivery-5 (at package-5 city-2-loc-7))
  (preference delivery-6 (at package-6 city-2-loc-6))
 ))
 (:metric maximize
   (- 1436
     (+ (total-cost)
       (* (is-violated delivery-1) 243)
       (* (is-violated delivery-2) 255)
       (* (is-violated delivery-3) 223)
       (* (is-violated delivery-4) 221)
       (* (is-violated delivery-5) 239)
       (* (is-violated delivery-6) 255)
     )
   )
 )
)
