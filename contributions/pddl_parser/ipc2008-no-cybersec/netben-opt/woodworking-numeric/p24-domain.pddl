;; Woodworking
;;

(define (domain woodworking)
  (:requirements :typing :numeric-fluents :goal-utilities)
  (:types
      acolour awood woodobj machine 
      surface treatmentstatus - object
      highspeed-saw glazer grinder immersion-varnisher
      planer saw spray-varnisher - machine
      board part - woodobj)

  (:constants
              verysmooth smooth rough - surface
              varnished glazed untreated colourfragments - treatmentstatus
              natural - acolour)

  (:predicates 
            (unused ?obj - part)
            (available ?obj - woodobj)

            (surface-condition ?obj - woodobj ?surface - surface)
            (treatment ?obj - part ?treatment - treatmentstatus)
            (colour ?obj - part ?colour - acolour)
            (wood ?obj - woodobj ?wood - awood)

            (in-highspeed-saw ?b - board ?m - highspeed-saw)
            (empty ?m - highspeed-saw)
            (has-colour ?machine - machine ?colour - acolour)
            (grind-treatment-change ?old ?new - treatmentstatus)
            (is-smooth ?surface - surface))
      
  (:functions
            (board-size ?board - board) - number
            (goal-size ?obj - part) - number
            (glaze-cost ?obj - part) - number
            (grind-cost ?obj - part) - number
            (plane-cost ?obj - part) - number
            (total-cost) - number)

  (:action do-immersion-varnish
    :parameters (?x - part ?m - immersion-varnisher 
                 ?newcolour - acolour ?surface - surface)
    :precondition (and
            (available ?x)
            (has-colour ?m ?newcolour)
            (surface-condition ?x ?surface)
            (is-smooth ?surface)
            (treatment ?x untreated))
    :effect (and
            (increase (total-cost) 10)
            (not (treatment ?x untreated))
            (treatment ?x varnished)
            (not (colour ?x natural))
            (colour ?x ?newcolour)))

  (:action do-spray-varnish
    :parameters (?x - part ?m - spray-varnisher 
                 ?newcolour - acolour ?surface - surface)
    :precondition (and
            (available ?x)
            (has-colour ?m ?newcolour)
            (surface-condition ?x ?surface)
            (is-smooth ?surface)
            (treatment ?x untreated))
    :effect (and 
            (increase (total-cost) (goal-size ?x))
            (not (treatment ?x untreated))
            (treatment ?x varnished)
            (not (colour ?x natural))
            (colour ?x ?newcolour)))

  (:action do-glaze
    :parameters (?x - part ?m - glazer 
                 ?newcolour - acolour)
    :precondition (and
            (available ?x)
            (has-colour ?m ?newcolour)
            (treatment ?x untreated))
    :effect (and 
            (increase (total-cost) (glaze-cost ?x))
            (not (treatment ?x untreated))
            (treatment ?x glazed)
            (not (colour ?x natural))
            (colour ?x ?newcolour)))

  (:action do-grind
    :parameters (?x - part ?m - grinder ?oldsurface - surface
                 ?oldcolour - acolour 
                 ?oldtreatment ?newtreatment - treatmentstatus) 
    :precondition (and 
            (available ?x)
            (surface-condition ?x ?oldsurface)
            (is-smooth ?oldsurface)
            (colour ?x ?oldcolour)
            (treatment ?x ?oldtreatment)
            (grind-treatment-change ?oldtreatment ?newtreatment))
    :effect (and
            (increase (total-cost) (grind-cost ?x))
            (not (surface-condition ?x ?oldsurface))
            (surface-condition ?x verysmooth)
            (not (treatment ?x ?oldtreatment))
            (treatment ?x ?newtreatment)
            (not (colour ?x ?oldcolour))
            (colour ?x natural)))

  (:action do-plane
    :parameters (?x - part ?m - planer ?oldsurface - surface
                 ?oldcolour - acolour ?oldtreatment - treatmentstatus) 
    :precondition (and 
            (available ?x)
            (surface-condition ?x ?oldsurface)
            (treatment ?x ?oldtreatment)
            (colour ?x ?oldcolour))
    :effect (and
            (increase (total-cost) (plane-cost ?x))
            (not (surface-condition ?x ?oldsurface))
            (surface-condition ?x smooth)
            (not (treatment ?x ?oldtreatment))
            (treatment ?x untreated)
            (not (colour ?x ?oldcolour))
            (colour ?x natural)))

  (:action load-highspeed-saw
    :parameters (?b - board ?m - highspeed-saw)
    :precondition (and
            (empty ?m)
            (available ?b))
    :effect (and
            (increase (total-cost) 30)
            (not (available ?b))
            (not (empty ?m))
            (in-highspeed-saw ?b ?m)))
            
  (:action unload-highspeed-saw
    :parameters (?b - board ?m - highspeed-saw)
    :precondition (in-highspeed-saw ?b ?m)
    :effect (and
            (increase (total-cost) 10)
            (available ?b)
            (not (in-highspeed-saw ?b ?m))
            (empty ?m)))
            
  (:action cut-board
    :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                 ?surface - surface)
    :precondition (and
            (unused ?p)
            (in-highspeed-saw ?b ?m)
            (wood ?b ?w)
            (surface-condition ?b ?surface)
            (>= (board-size ?b) (goal-size ?p)))
    :effect (and
            (increase (total-cost) 10)
            (not (unused ?p))
            (decrease (board-size ?b) (goal-size ?p))
            (available ?p)
            (wood ?p ?w)
            (colour ?p natural)
            (surface-condition ?p ?surface)
            (treatment ?p untreated)))

  (:action do-saw
    :parameters (?b - board ?p - part ?m - saw ?w - awood
                 ?surface - surface)
    :precondition (and 
            (unused ?p)
            (available ?b)
            (wood ?b ?w)
            (surface-condition ?b ?surface)
            (>= (board-size ?b) (goal-size ?p)))
    :effect (and
            (increase (total-cost) 30)
            (not (unused ?p))
            (decrease (board-size ?b) (goal-size ?p))
            (available ?p)
            (wood ?p ?w)
            (surface-condition ?p ?surface)
            (colour ?p natural) 
            (treatment ?p untreated)))
)
