(define (problem CrewPlanning_2crew_2day)
(:domain CrewPlanning)
(:objects
	d1 d2 d3 - Day

	c1 c2 - CrewMember
	mcs1 mcs2 - MedicalState

	spaceshipFilter - FilterState

	rpcm1 - RPCM

	pa1 pa2 pa3 pa4 pa5 pa6 pa7 pa8 pa9 pa10 pa11 pa12 pa13 pa14 - PayloadAct
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
	(active c2 d1)
	(= (available_time c2 d1) 1440)
	(active c2 d2)
	(= (available_time c2 d2) 1440)

	(= (crew_efficiency c1 d1) 8)
	(= (crew_efficiency c1 d2) 7)
	(= (crew_efficiency c2 d1) 9)
	(= (crew_efficiency c2 d2) 9)

	(= (achieve_time_discount d1) 0)
	(= (achieve_time_discount d2) 10)

	(= (payloadact_length pa1) 160)
	(= (payloadact_length pa2) 160)
	(= (payloadact_length pa3) 90)
	(= (payloadact_length pa4) 160)
	(= (payloadact_length pa5) 100)
	(= (payloadact_length pa6) 110)
	(= (payloadact_length pa7) 90)
	(= (payloadact_length pa8) 100)
	(= (payloadact_length pa9) 90)
	(= (payloadact_length pa10) 150)
	(= (payloadact_length pa11) 150)
	(= (payloadact_length pa12) 80)
	(= (payloadact_length pa13) 130)
	(= (payloadact_length pa14) 140)

)

(:goal
(and
	(preference dr (currentday d3))
	(preference mcs1_2 (mcs_finished mcs1 d2))
	(preference mcs2_2 (mcs_finished mcs2 d2))

	(preference filter2 (changed spaceshipFilter d2))

	(preference rpcm_pref (done_rpcm rpcm1))

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
	(preference pref_pa12 (payload_act_completed pa12))
	(preference pref_pa13 (payload_act_completed pa13))
	(preference pref_pa14 (payload_act_completed pa14))
)
)
(:metric maximize
	( - 9275
	(+ (total-cost)
	(* (is-violated dr) 5490)
	    (* (is-violated mcs1_2) 180)
	    (* (is-violated mcs2_2) 180)
	    (* (is-violated filter2) 180)
	    (* (is-violated rpcm_pref) 620)
	    (* (is-violated pref_pa1) 240)
	    (* (is-violated pref_pa2) 320)
	    (* (is-violated pref_pa3) 135)
	    (* (is-violated pref_pa4) 320)
	    (* (is-violated pref_pa5) 150)
	    (* (is-violated pref_pa6) 165)
	    (* (is-violated pref_pa7) 180)
	    (* (is-violated pref_pa8) 100)
	    (* (is-violated pref_pa9) 180)
	    (* (is-violated pref_pa10) 225)
	    (* (is-violated pref_pa11) 150)
	    (* (is-violated pref_pa12) 120)
	    (* (is-violated pref_pa13) 130)
	    (* (is-violated pref_pa14) 210)
)))
)
