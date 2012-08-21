(define (domain CrewPlanning)
(:requirements :typing :numeric-fluents :goal-utilities)
(:types MedicalState FilterState CrewMember PayloadAct Day RPCM - objects)

(:predicates
	(mcs_finished ?ps - MedicalState ?d - Day)

	(changed ?fs - FilterState ?d - Day)

	;; Predicates to order actions in CrewPlanner's DailyRoutine HTN schema
	(done_meal  ?c - CrewMember ?d - Day)
	(done_exercise  ?c - CrewMember ?d - Day)
	(not_sleeping ?c - CrewMember)

	
	;; Predicates to order actions in RPCM's Perform HTN schema
	(done_first_reconfigure_thermal_loop ?r - RPCM)
	(done_remove_sleep_station ?r - RPCM)
	(done_replace_rpcm ?r - RPCM)
	(done_assemble_sleep_station ?r - RPCM)
	(done_second_reconfigurate_thermal_loops ?r - RPCM)
	(done_rpcm ?r - RPCM)

	;; To indicate that a PayloadActivity is done
	(payload_act_completed ?pa - PayloadAct)

	;; Day concept to approximate the temporal constraints on actions/goals
	(currentday ?d - Day)
	(active ?c - CrewMember  ?d - day)
	(next ?d1 ?d2 - Day)
)

(:functions
	;; specify the total available time of each crew member
	(available_time ?c - CrewMember ?d - Day) - number
	(crew_efficiency ?c - CrewMember ?d - Day) - number
	;; shoud be within [0,1] but to ensure integer constraint
	(achieve_time_discount ?d - Day) - number
	(payloadact_length ?pa - PayloadAct) - number
	(total-cost) - number)



;;
;; Daily routine by CrewPlanner (start the day with "post_sleep")
;;
;; Proper encoding needs to add "clip" actions to concatenate different days

;; This make sure that only move to the second day when all the
;; crew members finish sleeping
(:action move_to_next_day
 :parameters (?d1 ?d2 - Day)
 :precondition (and (currentday ?d1)
		(next ?d1 ?d2)
		(forall (?c - CrewMember) (not (active ?c ?d1))))
 :effect (and (not (currentday ?d1))
	(currentday ?d2))
)

;; combination of the two original actions: post-sleep + dpc
(:action post_sleep
 :parameters (?c - CrewMember ?d - Day)
 :precondition (and  (currentday ?d)
	        (>= (available_time ?c ?d) 195))
 :effect (and  (not_sleeping ?c)
	   (increase (total-cost) 195)
	   (decrease (available_time ?c ?d) 195))
)


;; do not model "pre-meal" in this track
(:action have_meal
 :parameters (?c - CrewMember ?d - Day)
 :precondition (and  (currentday ?d)
		(not_sleeping ?c)
	        (>= (available_time ?c ?d) 60))
 :effect (and  (done_meal ?c ?d)
	   (increase (total-cost) 60)
	   (decrease (available_time ?c ?d) 60))
)

;; do not model "pre-exercise" in this track
(:action exercise
 :parameters (?c - CrewMember ?d - Day)
 :precondition (and  (currentday ?d)
		(not_sleeping ?c)
	        (>= (available_time ?c ?d) 60))
 :effect (and  (done_exercise ?c ?d)
	   (increase (total-cost) 60)
	   (decrease (available_time ?c ?d) 60))
)


;; combination of two original actions: pre-sleep + sleep
(:action sleep
 :parameters (?c - CrewMember ?d - Day)
 :precondition (and  (currentday ?d)
		(not_sleeping ?c)
		(done_exercise ?c ?d)
		(done_meal ?c ?d)
	        (>= (available_time ?c ?d) 600))
 :effect (and  (not (active ?c ?d))
	   (not (not_sleeping ?c))
	   (increase (total-cost) 600)
	   (decrease (available_time ?c ?d) 600))
)


;; filter needs to be changed; those days are separated by 24-48 hours
(:action change_filter
 :parameters (?fs - FilterState  ?c - CrewMember ?d - Day)
 :precondition (and  (currentday ?d)
		(not_sleeping ?c)
	         (>= (available_time ?c ?d) 60))
 :effect (and  (changed ?fs ?d)
	    (increase (total-cost) (* 6 (crew_efficiency ?c ?d)))
	    (decrease (available_time ?c ?d) 60))
)


;; Need to do the same thing for "change_filter"
(:action medical_conference
 :parameters (?ps - MedicalState ?c - CrewMember ?d - Day)
 :precondition (and (currentday ?d)
	        (not_sleeping ?c)
	        (>= (available_time ?c ?d) 60))
 :effect (and (mcs_finished ?ps ?d)
	(increase (total-cost) 60)
	(decrease (available_time ?c ?d) 60))
)


(:action conduct_payload_activity
 :parameters (?pa - PayloadAct ?c - CrewMember ?d - Day)
 :precondition (and (currentday ?d)
		(not_sleeping ?c)
		(>= (available_time ?c ?d) (payloadact_length ?pa)))
 :effect (and (payload_act_completed ?pa)
	(increase (total-cost)
                  (* (/ (payloadact_length ?pa) 10)
		(+ (crew_efficiency ?c ?d) (achieve_time_discount ?d))))
	(decrease (available_time ?c ?d) (payloadact_length ?pa)))
)


;;
;; RPCM R&R Actions
;;

(:action first_reconfigurate_thermal_loops
 :parameters (?r - RPCM ?c - CrewMember ?d - Day)
 :precondition (and (>= (available_time ?c ?d) 60)
		(not_sleeping ?c)
		(currentday ?d))
 :effect (and (done_first_reconfigure_thermal_loop ?r)
	    (increase (total-cost) (* 6 (crew_efficiency ?c ?d)))
	    (decrease (available_time ?c ?d) 60))
)


(:action remove_sleep_station
 :parameters (?r - RPCM ?c - CrewMember ?d - Day)
 :precondition (and (>= (available_time ?c ?d) 60)
		(not_sleeping ?c)
		(currentday ?d))
 :effect (and (done_remove_sleep_station ?r)
	    (increase (total-cost) (* 6 (crew_efficiency ?c ?d)))
	    (decrease (available_time ?c ?d) 60))
)


;; this combine three original sequential actions:
;; remove_power_source + replace_rpcm + place_power_source
(:action replace_rpcm
 :parameters (?r - RPCM ?c - CrewMember ?d - Day)
 :precondition (and (>= (available_time ?c ?d) 180)
		(not_sleeping ?c)
		(currentday ?d)
		(done_first_reconfigure_thermal_loop ?r)
		(done_remove_sleep_station ?r))
 :effect (and (done_replace_rpcm ?r)
	    (increase (total-cost) (* 18 (crew_efficiency ?c ?d)))
	    (decrease (available_time ?c ?d) 180))
)


(:action assemble_sleep_station
 :parameters (?r - RPCM ?c - CrewMember ?d - Day)
 :precondition (and (>= (available_time ?c ?d) 60)
		(not_sleeping ?c)
		(currentday ?d)
		(done_replace_rpcm ?r))
 :effect (and (done_assemble_sleep_station ?r)
	    (increase (total-cost) (* 6 (crew_efficiency ?c ?d)))
	    (decrease (available_time ?c ?d) 60))
)


(:action second_reconfigurate_thermal_loops
 :parameters (?r - RPCM ?c - CrewMember ?d - Day)
 :precondition (and (>= (available_time ?c ?d) 60)
		(not_sleeping ?c)
		(currentday ?d)
		(done_replace_rpcm ?r))
 :effect (and (done_second_reconfigurate_thermal_loops ?r)
	    (increase (total-cost) (* 6 (crew_efficiency ?c ?d)))
	    (decrease (available_time ?c ?d) 60))
)


(:action finish_rpcm
 :parameters (?r - RPCM ?d - Day)
 :precondition (and (currentday ?d)
		(done_second_reconfigurate_thermal_loops ?r)
		(done_assemble_sleep_station ?r))
 :effect (and (done_rpcm ?r)
	(increase (total-cost) (* 20 (achieve_time_discount ?d))))
)
)

;; EOF
