; woodworking 'choose one' task with 3 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 166828

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    black red - acolour
    beech cherry - awood
    p0 p1 p2 - part
    b0 b1 - board
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    (= (total-cost) 0)
    (has-colour glazer0 black)
    (has-colour glazer0 natural)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 black)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 black)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (= (goal-size p0) 7)
    (= (glaze-cost p0) 12)
    (= (grind-cost p0) 21)
    (= (plane-cost p0) 14)
    (unused p1)
    (= (goal-size p1) 13)
    (= (glaze-cost p1) 18)
    (= (grind-cost p1) 39)
    (= (plane-cost p1) 26)
    (unused p2)
    (= (goal-size p2) 6)
    (= (glaze-cost p2) 11)
    (= (grind-cost p2) 18)
    (= (plane-cost p2) 12)
    (= (board-size b0) 27)
    (wood b0 beech)
    (surface-condition b0 smooth)
    (available b0)
    (= (board-size b1) 25)
    (wood b1 cherry)
    (surface-condition b1 smooth)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (colour p0 black)
          (wood p0 beech)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p0_1 (and
          (colour p0 natural)
          (wood p0 beech)
          (treatment p0 varnished)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 red)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p1_1 (and
          (colour p1 natural)
          (wood p1 beech)
          (surface-condition p1 verysmooth)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p1_2 (and
          (colour p1 red)
          (surface-condition p1 smooth)
          (available p1)
      ))
      (preference g_p1_3 (and
          (colour p1 black)
          (wood p1 cherry)
          (surface-condition p1 verysmooth)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p2_0 (and
          (wood p2 cherry)
          (surface-condition p2 verysmooth)
          (treatment p2 varnished)
          (available p2)
      ))
      (preference g_p2_1 (and
          (colour p2 black)
          (wood p2 cherry)
          (available p2)
      ))
      (preference g_p2_2 (and
          (colour p2 natural)
          (surface-condition p2 verysmooth)
          (available p2)
      ))
    )
  )
  (:metric maximize
    (- 712
      (+ (total-cost)
         (* (is-violated g_p0_0) 48)
         (* (is-violated g_p0_1) 53)
         (* (is-violated g_p1_0) 59)
         (* (is-violated g_p1_1) 125)
         (* (is-violated g_p1_2) 104)
         (* (is-violated g_p1_3) 118)
         (* (is-violated g_p2_0) 81)
         (* (is-violated g_p2_1) 51)
         (* (is-violated g_p2_2) 73)
  )))
)
