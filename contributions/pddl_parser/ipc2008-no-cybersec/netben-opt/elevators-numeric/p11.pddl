(define (problem elevators-netbenefit-p12_3_3)
(:domain elevators-netbenefit-numeric)

(:objects 
f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12  - floor
p0 p1 p2  - passenger
fast0  - fast-elevator
slow0-0 slow1-0 - slow-elevator
)

(:init
(above f0 f1) (above f0 f2) (above f0 f3) (above f0 f4) (above f0 f5) (above f0 f6) (above f0 f7) (above f0 f8) (above f0 f9) (above f0 f10) (above f0 f11) (above f0 f12) 
(above f1 f2) (above f1 f3) (above f1 f4) (above f1 f5) (above f1 f6) (above f1 f7) (above f1 f8) (above f1 f9) (above f1 f10) (above f1 f11) (above f1 f12) 
(above f2 f3) (above f2 f4) (above f2 f5) (above f2 f6) (above f2 f7) (above f2 f8) (above f2 f9) (above f2 f10) (above f2 f11) (above f2 f12) 
(above f3 f4) (above f3 f5) (above f3 f6) (above f3 f7) (above f3 f8) (above f3 f9) (above f3 f10) (above f3 f11) (above f3 f12) 
(above f4 f5) (above f4 f6) (above f4 f7) (above f4 f8) (above f4 f9) (above f4 f10) (above f4 f11) (above f4 f12) 
(above f5 f6) (above f5 f7) (above f5 f8) (above f5 f9) (above f5 f10) (above f5 f11) (above f5 f12) 
(above f6 f7) (above f6 f8) (above f6 f9) (above f6 f10) (above f6 f11) (above f6 f12) 
(above f7 f8) (above f7 f9) (above f7 f10) (above f7 f11) (above f7 f12) 
(above f8 f9) (above f8 f10) (above f8 f11) (above f8 f12) 
(above f9 f10) (above f9 f11) (above f9 f12) 
(above f10 f11) (above f10 f12) 
(above f11 f12) 

(lift-at fast0 f6)
(= (passengers fast0) 0)
(= (capacity fast0) 3)
(reachable-floor fast0 f0)(reachable-floor fast0 f3)(reachable-floor fast0 f6)(reachable-floor fast0 f9)(reachable-floor fast0 f12)

(lift-at slow0-0 f6)
(= (passengers slow0-0) 0)
(= (capacity slow0-0) 2)
(reachable-floor slow0-0 f0)(reachable-floor slow0-0 f1)(reachable-floor slow0-0 f2)(reachable-floor slow0-0 f3)(reachable-floor slow0-0 f4)(reachable-floor slow0-0 f5)(reachable-floor slow0-0 f6)

(lift-at slow1-0 f8)
(= (passengers slow1-0) 0)
(= (capacity slow1-0) 2)
(reachable-floor slow1-0 f6)(reachable-floor slow1-0 f7)(reachable-floor slow1-0 f8)(reachable-floor slow1-0 f9)(reachable-floor slow1-0 f10)(reachable-floor slow1-0 f11)(reachable-floor slow1-0 f12)

(passenger-at p0 f0)
(passenger-at p1 f0)
(passenger-at p2 f2)

(= (travel-slow f0 f1) 6) (= (travel-slow f0 f2) 7) (= (travel-slow f0 f3) 8) (= (travel-slow f0 f4) 9) (= (travel-slow f0 f5) 10) (= (travel-slow f0 f6) 11) (= (travel-slow f1 f2) 6) (= (travel-slow f1 f3) 7) (= (travel-slow f1 f4) 8) (= (travel-slow f1 f5) 9) (= (travel-slow f1 f6) 10) (= (travel-slow f2 f3) 6) (= (travel-slow f2 f4) 7) (= (travel-slow f2 f5) 8) (= (travel-slow f2 f6) 9) (= (travel-slow f3 f4) 6) (= (travel-slow f3 f5) 7) (= (travel-slow f3 f6) 8) (= (travel-slow f4 f5) 6) (= (travel-slow f4 f6) 7) (= (travel-slow f5 f6) 6) 

(= (travel-slow f6 f7) 6) (= (travel-slow f6 f8) 7) (= (travel-slow f6 f9) 8) (= (travel-slow f6 f10) 9) (= (travel-slow f6 f11) 10) (= (travel-slow f6 f12) 11) (= (travel-slow f7 f8) 6) (= (travel-slow f7 f9) 7) (= (travel-slow f7 f10) 8) (= (travel-slow f7 f11) 9) (= (travel-slow f7 f12) 10) (= (travel-slow f8 f9) 6) (= (travel-slow f8 f10) 7) (= (travel-slow f8 f11) 8) (= (travel-slow f8 f12) 9) (= (travel-slow f9 f10) 6) (= (travel-slow f9 f11) 7) (= (travel-slow f9 f12) 8) (= (travel-slow f10 f11) 6) (= (travel-slow f10 f12) 7) (= (travel-slow f11 f12) 6) 


(= (travel-fast f0 f3) 10) (= (travel-fast f0 f6) 19) (= (travel-fast f0 f9) 28) (= (travel-fast f0 f12) 37) 

(= (travel-fast f3 f6) 10) (= (travel-fast f3 f9) 19) (= (travel-fast f3 f12) 28) 

(= (travel-fast f6 f9) 10) (= (travel-fast f6 f12) 19) 

(= (travel-fast f9 f12) 10) 

(= (total-cost) 0)

)

(:goal
(and
(preference served0 (passenger-at p0 f3)) 
(preference served1 (passenger-at p1 f11)) 
(preference served2 (passenger-at p2 f7)) 
))

(:metric maximize (- 620 (+ (total-cost)
(* (is-violated served0) 36)
(* (is-violated served1) 484)
(* (is-violated served2) 100)
)))

)
