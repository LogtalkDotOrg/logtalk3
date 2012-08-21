(define (problem CrewPlanning_2crew_1day)
(:domain CrewPlanning)
(:objects
	d1 d2 - Day

	c1 c2 - CrewMember
	mcs1 mcs2 - MedicalState

	spaceshipFilter - FilterState

	pa1 pa2 pa3 pa4 pa5 pa6 pa7 pa8 pa9 pa10 pa11 - PayloadAct
)
(:init
	(= (total-cost) 0)
	(currentday d1)
	(next d1 d2)

	(active c1 d1)
	(= (available_time c1 d1) 1440)
	(active c2 d1)
	(= (available_time c2 d1) 1440)

	(= (crew_efficiency c1 d1) 8)
	(= (crew_efficiency c2 d1) 9)

	(= (achieve_time_discount d1) 0)

	(= (payloadact_length pa1) 140)
	(= (payloadact_length pa2) 80)
	(= (payloadact_length pa3) 140)
	(= (payloadact_length pa4) 130)
	(= (payloadact_length pa5) 160)
	(= (payloadact_length pa6) 130)
	(= (payloadact_length pa7) 90)
	(= (payloadact_length pa8) 90)
	(= (payloadact_length pa9) 140)
	(= (payloadact_length pa10) 110)
	(= (payloadact_length pa11) 130)

)

(:goal
(and
	(preference dr (currentday d2))

	(preference filter1 (changed spaceshipFilter d1))


	(preference pref_pa1 (payload_act_completed pa1))
	(preference pref_pa2 (payload_act_completed pa2))
	(preference pref_pa3 (payload_act_completed pa3))
	(preference pref_pa4 (payload_act_completed pa4))
	(preference pref_pa5 (payload_act_completed pa5))
	(preference pref_pa6 (payload_act_completed pa6))
	(preference pref_pa7 (payload_act_completed pa7))
	(preference pref_pa8 (payload_act_completed pa8))
	(preference pref_pa9 (payload_act_completed pa9))
	(preference pref_pa10 (payload_act_completed pa10))
	(preference pref_pa11 (payload_act_completed pa11))
)
)
(:metric maximize
	( - 3945
	(+ (total-cost)
	(* (is-violated dr) 2745)
	    (* (is-violated filter1) 180)
	    (* (is-violated pref_pa1) 140)
	    (* (is-violated pref_pa2) 40)
	    (* (is-violated pref_pa3) 140)
	    (* (is-violated pref_pa4) 130)
	    (* (is-violated pref_pa5) 160)
	    (* (is-violated pref_pa6) 130)
	    (* (is-violated pref_pa7) 45)
	    (* (is-violated pref_pa8) 45)
	    (* (is-violated pref_pa9) 70)
	    (* (is-violated pref_pa10) 55)
	    (* (is-violated pref_pa11) 65)
)))
)
