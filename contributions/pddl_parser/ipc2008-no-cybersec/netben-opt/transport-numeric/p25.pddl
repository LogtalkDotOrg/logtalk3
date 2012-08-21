; Transport two-cities-netbenefit-7nodes-700size-3degree-70mindistance-2trucks-4packages-6024seed

(define (problem transport-two-cities-netbenefit-7nodes-700size-3degree-70mindistance-2trucks-4packages-6024seed)
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
  truck-1 - vehicle
  truck-2 - vehicle
  package-1 - package
  package-2 - package
  package-3 - package
  package-4 - package
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
  ; 288,57 -> 142,100
  (road city-1-loc-4 city-1-loc-1)
  (= (road-length city-1-loc-4 city-1-loc-1) 16)
  (= (fuel-demand city-1-loc-4 city-1-loc-1) 31)
  ; 142,100 -> 288,57
  (road city-1-loc-1 city-1-loc-4)
  (= (road-length city-1-loc-1 city-1-loc-4) 16)
  (= (fuel-demand city-1-loc-1 city-1-loc-4) 31)
  ; 646,343 -> 631,203
  (road city-1-loc-5 city-1-loc-3)
  (= (road-length city-1-loc-5 city-1-loc-3) 15)
  (= (fuel-demand city-1-loc-5 city-1-loc-3) 29)
  ; 631,203 -> 646,343
  (road city-1-loc-3 city-1-loc-5)
  (= (road-length city-1-loc-3 city-1-loc-5) 15)
  (= (fuel-demand city-1-loc-3 city-1-loc-5) 29)
  ; 82,330 -> 142,100
  (road city-1-loc-6 city-1-loc-1)
  (= (road-length city-1-loc-6 city-1-loc-1) 24)
  (= (fuel-demand city-1-loc-6 city-1-loc-1) 48)
  ; 142,100 -> 82,330
  (road city-1-loc-1 city-1-loc-6)
  (= (road-length city-1-loc-1 city-1-loc-6) 24)
  (= (fuel-demand city-1-loc-1 city-1-loc-6) 48)
  ; 82,330 -> 105,617
  (road city-1-loc-6 city-1-loc-2)
  (= (road-length city-1-loc-6 city-1-loc-2) 29)
  (= (fuel-demand city-1-loc-6 city-1-loc-2) 58)
  ; 105,617 -> 82,330
  (road city-1-loc-2 city-1-loc-6)
  (= (road-length city-1-loc-2 city-1-loc-6) 29)
  (= (fuel-demand city-1-loc-2 city-1-loc-6) 58)
  ; 509,220 -> 631,203
  (road city-1-loc-7 city-1-loc-3)
  (= (road-length city-1-loc-7 city-1-loc-3) 13)
  (= (fuel-demand city-1-loc-7 city-1-loc-3) 25)
  ; 631,203 -> 509,220
  (road city-1-loc-3 city-1-loc-7)
  (= (road-length city-1-loc-3 city-1-loc-7) 13)
  (= (fuel-demand city-1-loc-3 city-1-loc-7) 25)
  ; 509,220 -> 288,57
  (road city-1-loc-7 city-1-loc-4)
  (= (road-length city-1-loc-7 city-1-loc-4) 28)
  (= (fuel-demand city-1-loc-7 city-1-loc-4) 55)
  ; 288,57 -> 509,220
  (road city-1-loc-4 city-1-loc-7)
  (= (road-length city-1-loc-4 city-1-loc-7) 28)
  (= (fuel-demand city-1-loc-4 city-1-loc-7) 55)
  ; 509,220 -> 646,343
  (road city-1-loc-7 city-1-loc-5)
  (= (road-length city-1-loc-7 city-1-loc-5) 19)
  (= (fuel-demand city-1-loc-7 city-1-loc-5) 37)
  ; 646,343 -> 509,220
  (road city-1-loc-5 city-1-loc-7)
  (= (road-length city-1-loc-5 city-1-loc-7) 19)
  (= (fuel-demand city-1-loc-5 city-1-loc-7) 37)
  ; 2006,306 -> 1955,96
  (road city-2-loc-4 city-2-loc-1)
  (= (road-length city-2-loc-4 city-2-loc-1) 22)
  (= (fuel-demand city-2-loc-4 city-2-loc-1) 44)
  ; 1955,96 -> 2006,306
  (road city-2-loc-1 city-2-loc-4)
  (= (road-length city-2-loc-1 city-2-loc-4) 22)
  (= (fuel-demand city-2-loc-1 city-2-loc-4) 44)
  ; 1981,218 -> 1955,96
  (road city-2-loc-5 city-2-loc-1)
  (= (road-length city-2-loc-5 city-2-loc-1) 13)
  (= (fuel-demand city-2-loc-5 city-2-loc-1) 25)
  ; 1955,96 -> 1981,218
  (road city-2-loc-1 city-2-loc-5)
  (= (road-length city-2-loc-1 city-2-loc-5) 13)
  (= (fuel-demand city-2-loc-1 city-2-loc-5) 25)
  ; 1981,218 -> 2006,306
  (road city-2-loc-5 city-2-loc-4)
  (= (road-length city-2-loc-5 city-2-loc-4) 10)
  (= (fuel-demand city-2-loc-5 city-2-loc-4) 19)
  ; 2006,306 -> 1981,218
  (road city-2-loc-4 city-2-loc-5)
  (= (road-length city-2-loc-4 city-2-loc-5) 10)
  (= (fuel-demand city-2-loc-4 city-2-loc-5) 19)
  ; 1435,217 -> 1608,274
  (road city-2-loc-6 city-2-loc-3)
  (= (road-length city-2-loc-6 city-2-loc-3) 19)
  (= (fuel-demand city-2-loc-6 city-2-loc-3) 37)
  ; 1608,274 -> 1435,217
  (road city-2-loc-3 city-2-loc-6)
  (= (road-length city-2-loc-3 city-2-loc-6) 19)
  (= (fuel-demand city-2-loc-3 city-2-loc-6) 37)
  ; 1859,347 -> 1955,96
  (road city-2-loc-7 city-2-loc-1)
  (= (road-length city-2-loc-7 city-2-loc-1) 27)
  (= (fuel-demand city-2-loc-7 city-2-loc-1) 54)
  ; 1955,96 -> 1859,347
  (road city-2-loc-1 city-2-loc-7)
  (= (road-length city-2-loc-1 city-2-loc-7) 27)
  (= (fuel-demand city-2-loc-1 city-2-loc-7) 54)
  ; 1859,347 -> 1785,630
  (road city-2-loc-7 city-2-loc-2)
  (= (road-length city-2-loc-7 city-2-loc-2) 30)
  (= (fuel-demand city-2-loc-7 city-2-loc-2) 59)
  ; 1785,630 -> 1859,347
  (road city-2-loc-2 city-2-loc-7)
  (= (road-length city-2-loc-2 city-2-loc-7) 30)
  (= (fuel-demand city-2-loc-2 city-2-loc-7) 59)
  ; 1859,347 -> 1608,274
  (road city-2-loc-7 city-2-loc-3)
  (= (road-length city-2-loc-7 city-2-loc-3) 27)
  (= (fuel-demand city-2-loc-7 city-2-loc-3) 53)
  ; 1608,274 -> 1859,347
  (road city-2-loc-3 city-2-loc-7)
  (= (road-length city-2-loc-3 city-2-loc-7) 27)
  (= (fuel-demand city-2-loc-3 city-2-loc-7) 53)
  ; 1859,347 -> 2006,306
  (road city-2-loc-7 city-2-loc-4)
  (= (road-length city-2-loc-7 city-2-loc-4) 16)
  (= (fuel-demand city-2-loc-7 city-2-loc-4) 31)
  ; 2006,306 -> 1859,347
  (road city-2-loc-4 city-2-loc-7)
  (= (road-length city-2-loc-4 city-2-loc-7) 16)
  (= (fuel-demand city-2-loc-4 city-2-loc-7) 31)
  ; 1859,347 -> 1981,218
  (road city-2-loc-7 city-2-loc-5)
  (= (road-length city-2-loc-7 city-2-loc-5) 18)
  (= (fuel-demand city-2-loc-7 city-2-loc-5) 36)
  ; 1981,218 -> 1859,347
  (road city-2-loc-5 city-2-loc-7)
  (= (road-length city-2-loc-5 city-2-loc-7) 18)
  (= (fuel-demand city-2-loc-5 city-2-loc-7) 36)
  ; 646,343 <-> 1435,217
  (road city-1-loc-5 city-2-loc-6)
  (= (road-length city-1-loc-5 city-2-loc-6) 80)
  (= (fuel-demand city-1-loc-5 city-2-loc-6) 40)
  (road city-2-loc-6 city-1-loc-5)
  (= (road-length city-2-loc-6 city-1-loc-5) 80)
  (= (fuel-demand city-2-loc-6 city-1-loc-5) 40)
  (has-petrol-station city-1-loc-5)
  (has-petrol-station city-2-loc-6)
  (at package-1 city-1-loc-4)
  (at package-2 city-1-loc-6)
  (at package-3 city-1-loc-3)
  (at package-4 city-1-loc-2)
  (at truck-1 city-2-loc-2)
  (= (fuel-left truck-1) 682)
  (= (fuel-max truck-1) 682)
  (capacity truck-1 capacity-3)
  (at truck-2 city-2-loc-4)
  (= (fuel-left truck-2) 682)
  (= (fuel-max truck-2) 682)
  (capacity truck-2 capacity-3)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-2-loc-3))
  (preference delivery-2 (at package-2 city-2-loc-4))
  (preference delivery-3 (at package-3 city-2-loc-3))
  (preference delivery-4 (at package-4 city-2-loc-6))
 ))
 (:metric maximize
   (- 1027
     (+ (total-cost)
       (* (is-violated delivery-1) 266)
       (* (is-violated delivery-2) 240)
       (* (is-violated delivery-3) 256)
       (* (is-violated delivery-4) 265)
     )
   )
 )
)
