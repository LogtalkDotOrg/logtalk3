;; Peg Solitaire sequential domain

(define (domain pegsolitaire-sequential)
    (:requirements :typing :action-costs)
    (:types location - object)
    (:predicates
        (IN-LINE ?x ?y ?z - location)
        (occupied ?l - location)
        (free ?l - location)
        (move-ended)
        (last-visited ?l - location)
    )
    (:functions (total-cost) - number)

    (:action jump-new-move
     :parameters (?from - location ?over - location ?to - location)
     :precondition (and 
                       (move-ended)
                       (IN-LINE ?from ?over ?to)
                       (occupied ?from)
                       (occupied ?over)
                       (free ?to)
                   )
     :effect (and
                 (not (move-ended))
                 (not (occupied ?from))
                 (not (occupied ?over))
                 (not (free ?to))
                 (free ?from)
                 (free ?over)
                 (occupied ?to)
                 (last-visited ?to)
                 (increase (total-cost) 1)
             )
    )

    (:action jump-continue-move
     :parameters (?from - location ?over - location ?to - location)
     :precondition (and 
                       (last-visited ?from)
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
                 (not (last-visited ?from))
                 (last-visited ?to)
             )
    )

    (:action end-move
     :parameters (?loc - location)
     :precondition (last-visited ?loc)
     :effect (and
                 (move-ended)
                 (not (last-visited ?loc))
             )
    )
)
