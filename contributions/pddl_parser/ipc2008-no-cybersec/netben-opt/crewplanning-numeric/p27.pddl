(define (problem CrewPlanning_2crew_2day)
(:domain CrewPlanning)
(:objects
	d1 d2 d3 - Day

	c1 c2 - CrewMember
	mcs1 mcs2 - MedicalState

	spaceshipFilter - FilterState

	pa1 pa2 pa3 pa4 pa5 pa6 pa7 pa8 pa9 pa10 pa11 pa12 pa13 pa14 pa15 pa16 pa17 pa18 - PayloadAct
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

	(= (crew_efficiency c1 d1) 7)
	(= (crew_efficiency c1 d2) 10)
	(= (crew_efficiency c2 d1) 8)
	(= (crew_efficiency c2 d2) 9)

	(= (achieve_time_discount d1) 0)
	(= (achieve_time_discount d2) 10)

	(= (payloadact_length pa1) 110)
	(= (payloadact_length pa2) 120)
	(= (payloadact_length pa3) 110)
	(= (payloadact_length pa4) 110)
	(= (payloadact_length pa5) 130)
	(= (payloadact_length pa6) 120)
	(= (payloadact_length pa7) 110)
	(= (payloadact_length pa8) 130)
	(= (payloadact_length pa9) 130)
	(= (payloadact_length pa10) 130)
	(= (payloadact_length pa11) 90)
	(= (payloadact_length pa12) 130)
	(= (payloadact_length pa13) 140)
	(= (payloadact_length pa14) 160)
	(= (payloadact_length pa15) 120)
	(= (payloadact_length pa16) 150)
	(= (payloadact_length pa17) 100)
	(= (payloadact_length pa18) 110)

)

(:goal
(and
	(preference dr (currentday d3))
	(preference mcs1_1 (mcs_finished mcs1 d1))
	(preference mcs1_2 (mcs_finished mcs1 d2))
	(preference mcs2_1 (mcs_finished mcs2 d1))
	(preference mcs2_2 (mcs_finished mcs2 d2))

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
	(preference pref_pa12 (payload_act_completed pa12))
	(preference pref_pa13 (payload_act_completed pa13))
	(preference pref_pa14 (payload_act_completed pa14))
	(preference pref_pa15 (payload_act_completed pa15))
	(preference pref_pa16 (payload_act_completed pa16))
	(preference pref_pa17 (payload_act_completed pa17))
	(preference pref_pa18 (payload_act_completed pa18))
)
)
(:metric maximize
	( - 10790
	(+ (total-cost)
	(* (is-violated dr) 5490)
	    (* (is-violated mcs1_1) 180)
	    (* (is-violated mcs1_2) 180)
	    (* (is-violated mcs2_1) 180)
	    (* (is-violated mcs2_2) 180)
	    (* (is-violated filter1) 180)
	    (* (is-violated pref_pa1) 220)
	    (* (is-violated pref_pa2) 240)
	    (* (is-violated pref_pa3) 220)
	    (* (is-violated pref_pa4) 220)
	    (* (is-violated pref_pa5) 260)
	    (* (is-violated pref_pa6) 240)
	    (* (is-violated pref_pa7) 220)
	    (* (is-violated pref_pa8) 260)
	    (* (is-violated pref_pa9) 260)
	    (* (is-violated pref_pa10) 260)
	    (* (is-violated pref_pa11) 180)
	    (* (is-violated pref_pa12) 260)
	    (* (is-violated pref_pa13) 280)
	    (* (is-violated pref_pa14) 320)
	    (* (is-violated pref_pa15) 240)
	    (* (is-violated pref_pa16) 300)
	    (* (is-violated pref_pa17) 200)
	    (* (is-violated pref_pa18) 220)
)))
)
