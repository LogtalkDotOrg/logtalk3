(define (problem CrewPlanning_1crew_1day)
(:domain CrewPlanning)
(:objects
	d1 d2 - Day

	c1 - CrewMember
	mcs1 - MedicalState

	spaceshipFilter - FilterState

	rpcm1 - RPCM

	)
(:init
	(= (total-cost) 0)
	(currentday d1)
	(next d1 d2)

	(active c1 d1)
	(= (available_time c1 d1) 1440)

	(= (crew_efficiency c1 d1) 8)

	(= (achieve_time_discount d1) 0)


)

(:goal
(and
	(preference dr (currentday d2))

	(preference filter1 (changed spaceshipFilter d1))

	(preference rpcm_pref (done_rpcm rpcm1))

)
)
(:metric maximize
	( - 3445
	(+ (total-cost)
	(* (is-violated dr) 2745)
	    (* (is-violated filter1) 180)
	    (* (is-violated rpcm_pref) 520)
)))
)
