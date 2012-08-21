; Transport city-netbenefit-1petrol-station-6nodes-1000size-3degree-100mindistance-2trucks-2packagespercity-4016seed

(define (problem transport-city-netbenefit-1petrol-station-6nodes-1000size-3degree-100mindistance-2trucks-2packagespercity-4016seed)
 (:domain transport)
 (:objects
  city-loc-1 - location
  city-loc-2 - location
  city-loc-3 - location
  city-loc-4 - location
  city-loc-5 - location
  city-loc-6 - location
  truck-1 - vehicle
  truck-2 - vehicle
  package-1 - package
  package-2 - package
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
  ; 421,724 -> 223,852
  (road city-loc-3 city-loc-2)
  (= (road-length city-loc-3 city-loc-2) 24)
  (= (fuel-demand city-loc-3 city-loc-2) 48)
  ; 223,852 -> 421,724
  (road city-loc-2 city-loc-3)
  (= (road-length city-loc-2 city-loc-3) 24)
  (= (fuel-demand city-loc-2 city-loc-3) 48)
  ; 41,715 -> 223,852
  (road city-loc-4 city-loc-2)
  (= (road-length city-loc-4 city-loc-2) 23)
  (= (fuel-demand city-loc-4 city-loc-2) 46)
  ; 223,852 -> 41,715
  (road city-loc-2 city-loc-4)
  (= (road-length city-loc-2 city-loc-4) 23)
  (= (fuel-demand city-loc-2 city-loc-4) 46)
  ; 41,715 -> 421,724
  (road city-loc-4 city-loc-3)
  (= (road-length city-loc-4 city-loc-3) 38)
  (= (fuel-demand city-loc-4 city-loc-3) 76)
  ; 421,724 -> 41,715
  (road city-loc-3 city-loc-4)
  (= (road-length city-loc-3 city-loc-4) 38)
  (= (fuel-demand city-loc-3 city-loc-4) 76)
  ; 113,594 -> 224,161
  (road city-loc-5 city-loc-1)
  (= (road-length city-loc-5 city-loc-1) 45)
  (= (fuel-demand city-loc-5 city-loc-1) 90)
  ; 224,161 -> 113,594
  (road city-loc-1 city-loc-5)
  (= (road-length city-loc-1 city-loc-5) 45)
  (= (fuel-demand city-loc-1 city-loc-5) 90)
  ; 113,594 -> 223,852
  (road city-loc-5 city-loc-2)
  (= (road-length city-loc-5 city-loc-2) 28)
  (= (fuel-demand city-loc-5 city-loc-2) 56)
  ; 223,852 -> 113,594
  (road city-loc-2 city-loc-5)
  (= (road-length city-loc-2 city-loc-5) 28)
  (= (fuel-demand city-loc-2 city-loc-5) 56)
  ; 113,594 -> 421,724
  (road city-loc-5 city-loc-3)
  (= (road-length city-loc-5 city-loc-3) 34)
  (= (fuel-demand city-loc-5 city-loc-3) 67)
  ; 421,724 -> 113,594
  (road city-loc-3 city-loc-5)
  (= (road-length city-loc-3 city-loc-5) 34)
  (= (fuel-demand city-loc-3 city-loc-5) 67)
  ; 113,594 -> 41,715
  (road city-loc-5 city-loc-4)
  (= (road-length city-loc-5 city-loc-4) 15)
  (= (fuel-demand city-loc-5 city-loc-4) 29)
  ; 41,715 -> 113,594
  (road city-loc-4 city-loc-5)
  (= (road-length city-loc-4 city-loc-5) 15)
  (= (fuel-demand city-loc-4 city-loc-5) 29)
  ; 72,910 -> 223,852
  (road city-loc-6 city-loc-2)
  (= (road-length city-loc-6 city-loc-2) 17)
  (= (fuel-demand city-loc-6 city-loc-2) 33)
  ; 223,852 -> 72,910
  (road city-loc-2 city-loc-6)
  (= (road-length city-loc-2 city-loc-6) 17)
  (= (fuel-demand city-loc-2 city-loc-6) 33)
  ; 72,910 -> 421,724
  (road city-loc-6 city-loc-3)
  (= (road-length city-loc-6 city-loc-3) 40)
  (= (fuel-demand city-loc-6 city-loc-3) 79)
  ; 421,724 -> 72,910
  (road city-loc-3 city-loc-6)
  (= (road-length city-loc-3 city-loc-6) 40)
  (= (fuel-demand city-loc-3 city-loc-6) 79)
  ; 72,910 -> 41,715
  (road city-loc-6 city-loc-4)
  (= (road-length city-loc-6 city-loc-4) 20)
  (= (fuel-demand city-loc-6 city-loc-4) 40)
  ; 41,715 -> 72,910
  (road city-loc-4 city-loc-6)
  (= (road-length city-loc-4 city-loc-6) 20)
  (= (fuel-demand city-loc-4 city-loc-6) 40)
  ; 72,910 -> 113,594
  (road city-loc-6 city-loc-5)
  (= (road-length city-loc-6 city-loc-5) 32)
  (= (fuel-demand city-loc-6 city-loc-5) 64)
  ; 113,594 -> 72,910
  (road city-loc-5 city-loc-6)
  (= (road-length city-loc-5 city-loc-6) 32)
  (= (fuel-demand city-loc-5 city-loc-6) 64)
  (at package-1 city-loc-6)
  (at package-2 city-loc-6)
  (has-petrol-station city-loc-1)
  (at truck-1 city-loc-1)
  (capacity truck-1 capacity-3)
  (= (fuel-left truck-1) 344)
  (= (fuel-max truck-1) 344)
  (at truck-2 city-loc-4)
  (capacity truck-2 capacity-3)
  (= (fuel-left truck-2) 344)
  (= (fuel-max truck-2) 344)
 )
 (:goal (and
  (preference delivery-1 (at package-1 city-loc-5))
  (preference delivery-2 (at package-2 city-loc-5))
 ))
 (:metric maximize
   (- 287
     (+ (total-cost)
       (* (is-violated delivery-1) 146)
       (* (is-violated delivery-2) 141)
     )
   )
 )
)
