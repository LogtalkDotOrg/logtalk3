; IPC5 Domain: Openstacks Net Benefit
; Author: Ioannis Refanidis (adapted from Patrik Haslum)

(define (domain openstacks-netbenefit-numeric-ADL)
  (:requirements :typing :adl :numeric-fluents :goal-utilities)
  (:types order product)
  (:predicates (includes ?o - order ?p - product)
	       (waiting ?o - order)
	       (started ?o - order)
	       (shipped ?o - order)
	       (delivered ?o - order ?p - product)
	       (made ?p - product))

(:functions (total-cost) 
	      (stacks-in-use)
	      (max-in-use)
	      (stack-cost) )
	       
  (:action open-new-stack
    :parameters ()
    :precondition (and)
    :effect (and (increase (max-in-use) 1)
		 (increase (total-cost) (stack-cost)))
    )
	      
   (:action start-order
    :parameters (?o - order)
    :precondition (and (waiting ?o)
		    (< (stacks-in-use) (max-in-use)))
    :effect (and (not (waiting ?o))
		 (started ?o)
		 (increase (stacks-in-use) 1))
    )
    
  (:action make-product
    :parameters (?p - product)
    :precondition (and (not (made ?p)))
    :effect (and (made ?p)
		 (forall (?o - order)
			 (when (and (includes ?o ?p)
				    (started ?o))
			   (delivered ?o ?p))))
    )
    
  (:action ship-order
    :parameters (?o - order)
    :precondition (and (started ?o))
    :effect (and (not (started ?o))
		 (shipped ?o)
		 (decrease (stacks-in-use) 1))
    )

  )
