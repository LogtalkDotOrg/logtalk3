(define (problem elevators-sequencedstrips-p8_9_1)
(:domain elevators-sequencedstrips)

(:objects 
n0 n1 n2 n3 n4 n5 n6 n7 n8  - count
p0 p1 p2 p3 p4 p5 p6 p7 p8  - passenger
fast0 fast1  - fast-elevator
slow0-0 slow1-0 - slow-elevator
)

(:init
(next n0 n1) (next n1 n2) (next n2 n3) (next n3 n4) (next n4 n5) (next n5 n6) (next n6 n7) (next n7 n8) 

(above n0 n1) (above n0 n2) (above n0 n3) (above n0 n4) (above n0 n5) (above n0 n6) (above n0 n7) (above n0 n8) 
(above n1 n2) (above n1 n3) (above n1 n4) (above n1 n5) (above n1 n6) (above n1 n7) (above n1 n8) 
(above n2 n3) (above n2 n4) (above n2 n5) (above n2 n6) (above n2 n7) (above n2 n8) 
(above n3 n4) (above n3 n5) (above n3 n6) (above n3 n7) (above n3 n8) 
(above n4 n5) (above n4 n6) (above n4 n7) (above n4 n8) 
(above n5 n6) (above n5 n7) (above n5 n8) 
(above n6 n7) (above n6 n8) 
(above n7 n8) 

(lift-at fast0 n8)
(passengers fast0 n0)
(can-hold fast0 n1) (can-hold fast0 n2) (can-hold fast0 n3) 
(reachable-floor fast0 n0)(reachable-floor fast0 n2)(reachable-floor fast0 n4)(reachable-floor fast0 n6)(reachable-floor fast0 n8)

(lift-at fast1 n6)
(passengers fast1 n0)
(can-hold fast1 n1) (can-hold fast1 n2) (can-hold fast1 n3) 
(reachable-floor fast1 n0)(reachable-floor fast1 n2)(reachable-floor fast1 n4)(reachable-floor fast1 n6)(reachable-floor fast1 n8)

(lift-at slow0-0 n0)
(passengers slow0-0 n0)
(can-hold slow0-0 n1) (can-hold slow0-0 n2) 
(reachable-floor slow0-0 n0)(reachable-floor slow0-0 n1)(reachable-floor slow0-0 n2)(reachable-floor slow0-0 n3)(reachable-floor slow0-0 n4)

(lift-at slow1-0 n8)
(passengers slow1-0 n0)
(can-hold slow1-0 n1) (can-hold slow1-0 n2) 
(reachable-floor slow1-0 n4)(reachable-floor slow1-0 n5)(reachable-floor slow1-0 n6)(reachable-floor slow1-0 n7)(reachable-floor slow1-0 n8)

(passenger-at p0 n5)
(passenger-at p1 n5)
(passenger-at p2 n2)
(passenger-at p3 n4)
(passenger-at p4 n7)
(passenger-at p5 n4)
(passenger-at p6 n2)
(passenger-at p7 n1)
(passenger-at p8 n1)

(= (travel-slow n0 n1) 6) (= (travel-slow n0 n2) 7) (= (travel-slow n0 n3) 8) (= (travel-slow n0 n4) 9) (= (travel-slow n1 n2) 6) (= (travel-slow n1 n3) 7) (= (travel-slow n1 n4) 8) (= (travel-slow n2 n3) 6) (= (travel-slow n2 n4) 7) (= (travel-slow n3 n4) 6) 

(= (travel-slow n4 n5) 6) (= (travel-slow n4 n6) 7) (= (travel-slow n4 n7) 8) (= (travel-slow n4 n8) 9) (= (travel-slow n5 n6) 6) (= (travel-slow n5 n7) 7) (= (travel-slow n5 n8) 8) (= (travel-slow n6 n7) 6) (= (travel-slow n6 n8) 7) (= (travel-slow n7 n8) 6) 


(= (travel-fast n0 n2) 7) (= (travel-fast n0 n4) 13) (= (travel-fast n0 n6) 19) (= (travel-fast n0 n8) 25) 

(= (travel-fast n2 n4) 7) (= (travel-fast n2 n6) 13) (= (travel-fast n2 n8) 19) 

(= (travel-fast n4 n6) 7) (= (travel-fast n4 n8) 13) 

(= (travel-fast n6 n8) 7) 

(= (total-cost) 0)

)

(:goal
(and
(passenger-at p0 n7)
(passenger-at p1 n0)
(passenger-at p2 n3)
(passenger-at p3 n7)
(passenger-at p4 n3)
(passenger-at p5 n7)
(passenger-at p6 n4)
(passenger-at p7 n4)
(passenger-at p8 n6)
))

(:metric minimize (total-cost))

)
