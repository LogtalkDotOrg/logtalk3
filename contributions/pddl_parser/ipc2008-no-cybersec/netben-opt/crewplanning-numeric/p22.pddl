(define (problem CrewPlanning_1crew_2day)
(:domain CrewPlanning)
(:objects
	d1 d2 d3 - Day

	c1 - CrewMember
	mcs1 - MedicalState

	spaceshipFilter - FilterState

	rpcm1 - RPCM

	pa1 pa2 pa3 pa4 pa5 pa6 - PayloadAct
)
(:init
	(= (total-cost) 0)
	(currentday d1)
	(next d1 d2)
	(next d2 d3)

	(active c1 d1)
	(= (available_time c1 d1) 1440)
	(active c1 d2)
	(= (available_time c1 d2) 1440)

	(= (crew_efficiency c1 d1) 8)
	(= (crew_efficiency c1 d2) 7)

	(= (achieve_time_discount d1) 0)
	(= (achieve_time_discount d2) 10)

	(= (payloadact_length pa1) 90)
	(= (payloadact_length pa2) 100)
	(= (payloadact_length pa3) 140)
	(= (payloadact_length pa4) 120)
	(= (payloadact_length pa5) 110)
	(= (payloadact_length pa6) 100)

)

(:goal
(and
	(preference dr (currentday d3))
	(preference mcs1_2 (mcs_finished mcs1 d2))

	(preference filter2 (changed spaceshipFilter d2))

	(preference rpcm_pref (done_rpcm rpcm1))

	(preference pref_pa1 (payload_act_completed pa1))
	(preference pref_pa2 (payload_act_completed pa2))
	(preference pref_pa3 (payload_act_completed pa3))
	(preference pref_pa4 (payload_act_completed pa4))
	(preference pref_pa5 (payload_act_completed pa5))
	(preference pref_pa6 (payload_act_completed pa6))
)
)
(:metric maximize
	( - 7580
	(+ (total-cost)
	(* (is-violated dr) 5490)
	    (* (is-violated mcs1_2) 180)
	    (* (is-violated filter2) 180)
	    (* (is-violated rpcm_pref) 620)
	    (* (is-violated pref_pa1) 180)
	    (* (is-violated pref_pa2) 200)
	    (* (is-violated pref_pa3) 280)
	    (* (is-violated pref_pa4) 240)
	    (* (is-violated pref_pa5) 110)
	    (* (is-violated pref_pa6) 100)
)))
)
