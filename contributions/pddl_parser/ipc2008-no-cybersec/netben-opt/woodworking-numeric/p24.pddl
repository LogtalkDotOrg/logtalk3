; woodworking 'not everything worthwhile' task with 6 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 968426

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
    blue green black red - acolour
    mahogany beech - awood
    p0 p1 p2 p3 p4 p5 - part
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
    (has-colour glazer0 green)
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
    (= (goal-size p1) 12)
    (= (glaze-cost p1) 17)
    (= (grind-cost p1) 36)
    (= (plane-cost p1) 24)
    (unused p2)
    (= (goal-size p2) 13)
    (= (glaze-cost p2) 18)
    (= (grind-cost p2) 39)
    (= (plane-cost p2) 26)
    (unused p3)
    (= (goal-size p3) 12)
    (= (glaze-cost p3) 17)
    (= (grind-cost p3) 36)
    (= (plane-cost p3) 24)
    (unused p4)
    (= (goal-size p4) 10)
    (= (glaze-cost p4) 15)
    (= (grind-cost p4) 30)
    (= (plane-cost p4) 20)
    (unused p5)
    (= (goal-size p5) 6)
    (= (glaze-cost p5) 11)
    (= (grind-cost p5) 18)
    (= (plane-cost p5) 12)
    (= (board-size b0) 25)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 35)
    (wood b1 mahogany)
    (surface-condition b1 smooth)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (colour p0 black)
          (wood p0 mahogany)
          (surface-condition p0 verysmooth)
          (treatment p0 varnished)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 red)
          (surface-condition p1 smooth)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 green)
          (surface-condition p2 smooth)
          (treatment p2 glazed)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 red)
          (wood p3 mahogany)
          (surface-condition p3 smooth)
          (available p3)
      ))
      (preference g_p4_0 (and
          (colour p4 black)
          (treatment p4 glazed)
          (available p4)
      ))
      (preference g_p5_0 (and
          (colour p5 natural)
          (wood p5 mahogany)
          (available p5)
      ))
    )
  )
  (:metric maximize
    (- 401
      (+ (total-cost)
         (* (is-violated g_p0_0) 62)
         (* (is-violated g_p1_0) 100)
         (* (is-violated g_p2_0) 85)
         (* (is-violated g_p3_0) 73)
         (* (is-violated g_p4_0) 38)
         (* (is-violated g_p5_0) 43)
  )))
)
