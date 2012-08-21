;; Peg Solitaire domain

(define (domain pegsolitaire-netbenefit)
    (:requirements :typing :goal-utilities)
    (:types location - object)
    (:predicates
        (IN-LINE ?x ?y ?z - location)
        (occupied ?l - location)
        (free ?l - location)
    )

    (:action jump
     :parameters (?from - location ?over - location ?to - location)
     :precondition (and 
                       (IN-LINE ?from ?over ?to)
                       (occupied ?from)
                       (occupied ?over)
                       (free ?to)
                   )
     :effect (and
                 (not (occupied ?from))
                 (not (occupied ?over))
                 (not (free ?to))
                 (free ?from)
                 (free ?over)
                 (occupied ?to)
             )
    )
)
