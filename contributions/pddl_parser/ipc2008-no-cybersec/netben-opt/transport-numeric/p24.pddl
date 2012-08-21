; Transport two-cities-netbenefit-6nodes-700size-3degree-70mindistance-2trucks-3packages-6024seed

(define (problem transport-two-cities-netbenefit-6nodes-700size-3degree-70mindistance-2trucks-3packages-6024seed)
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
  ; 390,532 -> 459,347
  (road city-1-loc-3 city-1-loc-2)
  (= (road-length city-1-loc-3 city-1-loc-2) 20)
  (= (fuel-demand city-1-loc-3 city-1-loc-2) 40)
  ; 459,347 -> 390,532
  (road city-1-loc-2 city-1-loc-3)
  (= (road-length city-1-loc-2 city-1-loc-3) 20)
  (= (fuel-demand city-1-loc-2 city-1-loc-3) 40)
  ; 263,105 -> 35,217
  (road city-1-loc-4 city-1-loc-1)
  (= (road-length city-1-loc-4 city-1-loc-1) 26)
  (= (fuel-demand city-1-loc-4 city-1-loc-1) 51)
  ; 35,217 -> 263,105
  (road city-1-loc-1 city-1-loc-4)
  (= (road-length city-1-loc-1 city-1-loc-4) 26)
  (= (fuel-demand city-1-loc-1 city-1-loc-4) 51)
  ; 263,105 -> 459,347
  (road city-1-loc-4 city-1-loc-2)
  (= (road-length city-1-loc-4 city-1-loc-2) 32)
  (= (fuel-demand city-1-loc-4 city-1-loc-2) 63)
  ; 459,347 -> 263,105
  (road city-1-loc-2 city-1-loc-4)
  (= (road-length city-1-loc-2 city-1-loc-4) 32)
  (= (fuel-demand city-1-loc-2 city-1-loc-4) 63)
  ; 127,449 -> 35,217
  (road city-1-loc-5 city-1-loc-1)
  (= (road-length city-1-loc-5 city-1-loc-1) 25)
  (= (fuel-demand city-1-loc-5 city-1-loc-1) 50)
  ; 35,217 -> 127,449
  (road city-1-loc-1 city-1-loc-5)
  (= (road-length city-1-loc-1 city-1-loc-5) 25)
  (= (fuel-demand city-1-loc-1 city-1-loc-5) 50)
  ; 127,449 -> 390,532
  (road city-1-loc-5 city-1-loc-3)
  (= (road-length city-1-loc-5 city-1-loc-3) 28)
  (= (fuel-demand city-1-loc-5 city-1-loc-3) 56)
  ; 390,532 -> 127,449
  (road city-1-loc-3 city-1-loc-5)
  (= (road-length city-1-loc-3 city-1-loc-5) 28)
  (= (fuel-demand city-1-loc-3 city-1-loc-5) 56)
  ; 340,297 -> 35,217
  (road city-1-loc-6 city-1-loc-1)
  (= (road-length city-1-loc-6 city-1-loc-1) 32)
  (= (fuel-demand city-1-loc-6 city-1-loc-1) 63)
  ; 35,217 -> 340,297
  (road city-1-loc-1 city-1-loc-6)
  (= (road-length city-1-loc-1 city-1-loc-6) 32)
  (= (fuel-demand city-1-loc-1 city-1-loc-6) 63)
  ; 340,297 -> 459,347
  (road city-1-loc-6 city-1-loc-2)
  (= (road-length city-1-loc-6 city-1-loc-2) 13)
  (= (fuel-demand city-1-loc-6 city-1-loc-2) 26)
  ; 459,347 -> 340,297
  (road city-1-loc-2 city-1-loc-6)
  (= (road-length city-1-loc-2 city-1-loc-6) 13)
  (= (fuel-demand city-1-loc-2 city-1-loc-6) 26)
  ; 340,297 -> 390,532
  (road city-1-loc-6 city-1-loc-3)
  (= (road-length city-1-loc-6 city-1-loc-3) 24)
  (= (fuel-demand city-1-loc-6 city-1-loc-3) 48)
  ; 390,532 -> 340,297
  (road city-1-loc-3 city-1-loc-6)
  (= (road-length city-1-loc-3 city-1-loc-6) 24)
  (= (fuel-demand city-1-loc-3 city-1-loc-6) 48)
  ; 340,297 -> 263,105
  (road city-1-loc-6 city-1-loc-4)
  (= (road-length city-1-loc-6 city-1-loc-4) 21)
  (= (fuel-demand city-1-loc-6 city-1-loc-4) 42)
  ; 263,105 -> 340,297
  (road city-1-loc-4 city-1-loc-6)
  (= (road-length city-1-loc-4 city-1-loc-6) 21)
  (= (fuel-demand city-1-loc-4 city-1-loc-6) 42)
  ; 340,297 -> 127,449
  (road city-1-loc-6 city-1-loc-5)
  (= (road-length city-1-loc-6 city-1-loc-5) 27)
  (= (fuel-demand city-1-loc-6 city-1-loc-5) 53)
  ; 127,449 -> 340,297
  (road city-1-loc-5 city-1-loc-6)
  (= (road-length city-1-loc-5 city-1-loc-6) 27)
  (= (fuel-demand city-1-loc-5 city-1-loc-6) 53)
  ; 1697,501 -> 1679,346
  (road city-2-loc-2 city-2-loc-1)
  (= (road-length city-2-loc-2 city-2-loc-1) 16)
  (= (fuel-demand city-2-loc-2 city-2-loc-1) 32)
  ; 1679,346 -> 1697,501
  (road city-2-loc-1 city-2-loc-2)
  (= (road-length city-2-loc-1 city-2-loc-2) 16)
  (= (fuel-demand city-2-loc-1 city-2-loc-2) 32)
  ; 1857,279 -> 1679,346
  (road city-2-loc-3 city-2-loc-1)
  (= (road-length city-2-loc-3 city-2-loc-1) 19)
  (= (fuel-demand city-2-loc-3 city-2-loc-1) 38)
  ; 1679,346 -> 1857,279
  (road city-2-loc-1 city-2-loc-3)
  (= (road-length city-2-loc-1 city-2-loc-3) 19)
  (= (fuel-demand city-2-loc-1 city-2-loc-3) 38)
  ; 1857,279 -> 1697,501
  (road city-2-loc-3 city-2-loc-2)
  (= (road-length city-2-loc-3 city-2-loc-2) 28)
  (= (fuel-demand city-2-loc-3 city-2-loc-2) 55)
  ; 1697,501 -> 1857,279
  (road city-2-loc-2 city-2-loc-3)
  (= (road-length city-2-loc-2 city-2-loc-3) 28)
  (= (fuel-demand city-2-loc-2 city-2-loc-3) 55)
  ; 1790,452 -> 1679,346
  (road city-2-loc-4 city-2-loc-1)
  (= (road-length city-2-loc-4 city-2-loc-1) 16)
  (= (fuel-demand city-2-loc-4 city-2-loc-1) 31)
  ; 1679,346 -> 1790,452
  (road city-2-loc-1 city-2-loc-4)
  (= (road-length city-2-loc-1 city-2-loc-4) 16)
  (= (fuel-demand city-2-loc-1 city-2-loc-4) 31)
  ; 1790,452 -> 1697,501
  (road city-2-loc-4 city-2-loc-2)
  (= (road-length city-2-loc-4 city-2-loc-2) 11)
  (= (fuel-demand city-2-loc-4 city-2-loc-2) 21)
  ; 1697,501 -> 1790,452
  (road city-2-loc-2 city-2-loc-4)
  (= (road-length city-2-loc-2 city-2-loc-4) 11)
  (= (fuel-demand city-2-loc-2 city-2-loc-4) 21)
  ; 1790,452 -> 1857,279
  (road city-2-loc-4 city-2-loc-3)
  (= (road-length city-2-loc-4 city-2-loc-3) 19)
  (= (fuel-demand city-2-loc-4 city-2-loc-3) 38)
  ; 1857,279 -> 1790,452
  (road city-2-loc-3 city-2-loc-4)
  (= (road-length city-2-loc-3 city-2-loc-4) 19)
  (= (fuel-demand city-2-loc-3 city-2-loc-4) 38)
  ; 1516,330 -> 1679,346
  (road city-2-loc-5 city-2-loc-1)
  (= (road-length city-2-loc-5 city-2-loc-1) 17)
  (= (fuel-demand city-2-loc-5 city-2-loc-1) 33)
  ; 1679,346 -> 1516,330
  (road city-2-loc-1 city-2-loc-5)
  (= (road-length city-2-loc-1 city-2-loc-5) 17)
  (= (fuel-demand city-2-loc-1 city-2-loc-5) 33)
  ; 1516,330 -> 1697,501
  (road city-2-loc-5 city-2-loc-2)
  (= (road-length city-2-loc-5 city-2-loc-2) 25)
  (= (fuel-demand city-2-loc-5 city-2-loc-2) 50)
  ; 1697,501 -> 1516,330
  (road city-2-loc-2 city-2-loc-5)
  (= (road-length city-2-loc-2 city-2-loc-5) 25)
  (= (fuel-demand city-2-loc-2 city-2-loc-5) 50)
  ; 1516,330 -> 1790,452
  (road city-2-loc-5 city-2-loc-4)
  (= (road-length city-2-loc-5 city-2-loc-4) 30)
  (= (fuel-demand city-2-loc-5 city-2-loc-4) 60)
  ; 1790,452 -> 1516,330
  (road city-2-loc-4 city-2-loc-5)
  (= (road-length city-2-loc-4 city-2-loc-5) 30)
  (= (fuel-demand city-2-loc-4 city-2-loc-5) 60)
  ; 1572,692 -> 1697,501
  (road city-2-loc-6 city-2-loc-2)
  (= (road-length city-2-loc-6 city-2-loc-2) 23)
  (= (fuel-demand city-2-loc-6 city-2-loc-2) 46)
  ; 1697,501 -> 1572,692
  (road city-2-loc-2 city-2-loc-6)
  (= (road-length city-2-loc-2 city-2-loc-6) 23)
  (= (fuel-demand city-2-loc-2 city-2-loc-6) 46)
  ; 1572,692 -> 1790,452
  (road city-2-loc-6 city-2-loc-4)
  (= (road-length city-2-loc-6 city-2-loc-4) 33)
  (= (fuel-demand city-2-loc-6 city-2-loc-4) 65)
  ; 1790,452 -> 1572,692
  (road city-2-loc-4 city-2-loc-6)
  (= (road-length city-2-loc-4 city-2-loc-6) 33)
  (= (fuel-demand city-2-loc-4 city-2-loc-6) 65)
  ; 459,347 <-> 1516,330
  (road city-1-loc-2 city-2-loc-5)
  (= (road-length city-1-loc-2 city-2-loc-5) 106)
  (= (fuel-demand city-1-loc-2 city-2-loc-5) 53)
  (road city-2-loc-5 city-1-loc-2)
  (= (road-length city-2-loc-5 city-1-loc-2) 106)
  (= (fuel-demand city-2-loc-5 city-1-loc-2) 53)
  (has-petrol-station city-1-loc-2)
  (has-petrol-station city-2-loc-5)
  (at package-1 city-1-loc-1)
  (at package-2 city-1-loc-5)
  (at package-3 city-1-loc-4)
  (at truck-1 city-2-loc-4)
  (= (fuel-left truck-1) 792)
  (= (fuel-max truck-1) 792)
  (capacity truck-1 capacity-4)
  (at truck-2 city-2-loc-5)
  (= (fuel-left truck-2) 792)
  (= (fuel-max truck-2) 792)
  (capacity truck-2 capacity-2)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-1))
  (preference delivery-2 (at package-2 city-2-loc-5))
  (preference delivery-3 (at package-3 city-2-loc-5))
 ))
 (:metric maximize
   (- 720
     (+ (total-cost)
       (* (is-violated delivery-1) 249)
       (* (is-violated delivery-2) 223)
       (* (is-violated delivery-3) 248)
     )
   )
 )
)
