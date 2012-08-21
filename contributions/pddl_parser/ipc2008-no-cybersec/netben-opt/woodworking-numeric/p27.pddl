; woodworking 'not everything worthwhile' task with 9 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 835905

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
    red mauve black green blue white - acolour
    oak pine - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 - part
    b0 b1 b2 - board
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    (= (total-cost) 0)
    (has-colour glazer0 mauve)
    (has-colour glazer0 green)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 green)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 green)
    (unused p0)
    (= (goal-size p0) 6)
    (= (glaze-cost p0) 11)
    (= (grind-cost p0) 18)
    (= (plane-cost p0) 12)
    (unused p1)
    (= (goal-size p1) 15)
    (= (glaze-cost p1) 20)
    (= (grind-cost p1) 45)
    (= (plane-cost p1) 30)
    (unused p2)
    (= (goal-size p2) 13)
    (= (glaze-cost p2) 18)
    (= (grind-cost p2) 39)
    (= (plane-cost p2) 26)
    (unused p3)
    (= (goal-size p3) 10)
    (= (glaze-cost p3) 15)
    (= (grind-cost p3) 30)
    (= (plane-cost p3) 20)
    (unused p4)
    (= (goal-size p4) 8)
    (= (glaze-cost p4) 13)
    (= (grind-cost p4) 24)
    (= (plane-cost p4) 16)
    (unused p5)
    (= (goal-size p5) 9)
    (= (glaze-cost p5) 14)
    (= (grind-cost p5) 27)
    (= (plane-cost p5) 18)
    (unused p6)
    (= (goal-size p6) 12)
    (= (glaze-cost p6) 17)
    (= (grind-cost p6) 36)
    (= (plane-cost p6) 24)
    (unused p7)
    (= (goal-size p7) 9)
    (= (glaze-cost p7) 14)
    (= (grind-cost p7) 27)
    (= (plane-cost p7) 18)
    (unused p8)
    (= (goal-size p8) 7)
    (= (glaze-cost p8) 12)
    (= (grind-cost p8) 21)
    (= (plane-cost p8) 14)
    (= (board-size b0) 33)
    (wood b0 oak)
    (surface-condition b0 smooth)
    (available b0)
    (= (board-size b1) 28)
    (wood b1 oak)
    (surface-condition b1 rough)
    (available b1)
    (= (board-size b2) 28)
    (wood b2 pine)
    (surface-condition b2 smooth)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 oak)
          (surface-condition p0 verysmooth)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p1_0 (and
          (surface-condition p1 smooth)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 blue)
          (wood p2 pine)
          (surface-condition p2 smooth)
          (treatment p2 varnished)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 green)
          (wood p3 oak)
          (treatment p3 glazed)
          (available p3)
      ))
      (preference g_p4_0 (and
          (colour p4 red)
          (wood p4 oak)
          (surface-condition p4 smooth)
          (treatment p4 glazed)
          (available p4)
      ))
      (preference g_p5_0 (and
          (wood p5 oak)
          (surface-condition p5 smooth)
          (treatment p5 glazed)
          (available p5)
      ))
      (preference g_p6_0 (and
          (colour p6 green)
          (surface-condition p6 smooth)
          (available p6)
      ))
      (preference g_p7_0 (and
          (colour p7 mauve)
          (wood p7 oak)
          (available p7)
      ))
      (preference g_p8_0 (and
          (wood p8 oak)
          (surface-condition p8 verysmooth)
          (treatment p8 glazed)
          (available p8)
      ))
    )
  )
  (:metric maximize
    (- 633
      (+ (total-cost)
         (* (is-violated g_p0_0) 66)
         (* (is-violated g_p1_0) 130)
         (* (is-violated g_p2_0) 71)
         (* (is-violated g_p3_0) 43)
         (* (is-violated g_p4_0) 83)
         (* (is-violated g_p5_0) 89)
         (* (is-violated g_p6_0) 69)
         (* (is-violated g_p7_0) 32)
         (* (is-violated g_p8_0) 50)
  )))
)
