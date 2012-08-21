;; openstacks, strips version

(define (domain openstacks-netbenefit-ADL)
  (:requirements :typing :negative-preconditions :action-costs :goal-utilities)
  (:types order product count)
  (:predicates 
           (includes ?o - order ?p - product)
	       (waiting ?o - order)
	       (started ?o - order)
	       (shipped ?o - order)
	       (made ?p - product)
	       (delivered ?o - order ?p - product)
	       (stacks-avail ?s - count)
	       (next-count ?s ?ns - count)
	       (making-product)
	       (making ?p - product) 
   )

  (:functions (total-cost) (stack-cost))
	
  (:action open-new-stack
    :parameters (?open ?new-open - count)
    :precondition (and (stacks-avail ?open) (not (making-product))
		       (next-count ?open ?new-open))
    :effect (and (not (stacks-avail ?open))
		 (stacks-avail ?new-open) (increase (total-cost) (stack-cost)))
    )
         
  (:action start-order
    :parameters (?o - order ?avail ?new-avail - count)
    :precondition (and (waiting ?o) (not (making-product))
		       (stacks-avail ?avail)
		       (next-count ?new-avail ?avail))
    :effect (and (not (waiting ?o))
		 (started ?o)
		 (not (stacks-avail ?avail))
		 (stacks-avail ?new-avail))
    )

 (:action start-making-product
   :parameters (?p - product)
   :precondition (and (not (made ?p)) (not (making-product)))  
   :effect (and (making-product) (making ?p))
 )

 (:action make-product-for-order
   :parameters (?p - product ?o - order)
   :precondition (and (started ?o) (includes ?o ?p) (making ?p) (not (delivered ?o ?p)) )  
   :effect (delivered ?o ?p)
 )

 (:action end-making-product
   :parameters (?p - product)
   :precondition (making ?p)
   :effect (and (not (making ?p)) (not (making-product)) (made ?p))
 )
 
 (:action ship-order
    :parameters (?o - order ?avail ?new-avail - count)
    :precondition (and (started ?o) (not (making-product))
		       (stacks-avail ?avail)
		       (next-count ?avail ?new-avail))
    :effect (and (not (started ?o))
		 (shipped ?o)
		 (not (stacks-avail ?avail))
		 (stacks-avail ?new-avail))
    )

  )

  

 
