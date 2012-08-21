(define (problem CrewPlanning_2crew_1day)
(:domain CrewPlanning)
(:objects
	d1 d2 - Day

	c1 c2 - CrewMember
	mcs1 mcs2 - MedicalState

	spaceshipFilter - FilterState

	rpcm1 - RPCM

	pa1 pa2 pa3 pa4 - PayloadAct
)
(:init
	(= (total-cost) 0)
	(currentday d1)
	(next d1 d2)

	(active c1 d1)
	(= (available_time c1 d1) 1440)
	(active c2 d1)
	(= (available_time c2 d1) 1440)

	(= (crew_efficiency c1 d1) 10)
	(= (crew_efficiency c2 d1) 9)

	(= (achieve_time_discount d1) 0)

	(= (payloadact_length pa1) 130)
	(= (payloadact_length pa2) 130)
	(= (payloadact_length pa3) 130)
	(= (payloadact_length pa4) 90)

)

(:goal
(and
	(preference dr (currentday d2))
	(preference mcs1_1 (mcs_finished mcs1 d1))

	(preference filter1 (changed spaceshipFilter d1))

	(preference rpcm_pref (done_rpcm rpcm1))

	(preference pref_pa1 (payload_act_completed pa1))
	(preference pref_pa2 (payload_act_completed pa2))
	(preference pref_pa3 (payload_act_completed pa3))
	(preference pref_pa4 (payload_act_completed pa4))
)
)
(:metric maximize
	( - 3910
	(+ (total-cost)
	(* (is-violated dr) 2745)
	    (* (is-violated mcs1_1) 180)
	    (* (is-violated filter1) 180)
	    (* (is-violated rpcm_pref) 520)
	    (* (is-violated pref_pa1) 65)
	    (* (is-violated pref_pa2) 65)
	    (* (is-violated pref_pa3) 65)
	    (* (is-violated pref_pa4) 90)
)))
)
