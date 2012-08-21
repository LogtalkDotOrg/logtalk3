; woodworking 'not everything worthwhile' task with 8 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 381115

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
    white mauve black green red blue - acolour
    teak mahogany - awood
    p0 p1 p2 p3 p4 p5 p6 p7 - part
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
    (has-colour glazer0 blue)
    (has-colour glazer0 mauve)
    (has-colour glazer0 white)
    (has-colour glazer0 green)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 mauve)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 mauve)
    (unused p0)
    (= (goal-size p0) 7)
    (= (glaze-cost p0) 12)
    (= (grind-cost p0) 21)
    (= (plane-cost p0) 14)
    (unused p1)
    (= (goal-size p1) 10)
    (= (glaze-cost p1) 15)
    (= (grind-cost p1) 30)
    (= (plane-cost p1) 20)
    (unused p2)
    (= (goal-size p2) 8)
    (= (glaze-cost p2) 13)
    (= (grind-cost p2) 24)
    (= (plane-cost p2) 16)
    (unused p3)
    (= (goal-size p3) 10)
    (= (glaze-cost p3) 15)
    (= (grind-cost p3) 30)
    (= (plane-cost p3) 20)
    (unused p4)
    (= (goal-size p4) 15)
    (= (glaze-cost p4) 20)
    (= (grind-cost p4) 45)
    (= (plane-cost p4) 30)
    (unused p5)
    (= (goal-size p5) 7)
    (= (glaze-cost p5) 12)
    (= (grind-cost p5) 21)
    (= (plane-cost p5) 14)
    (unused p6)
    (= (goal-size p6) 10)
    (= (glaze-cost p6) 15)
    (= (grind-cost p6) 30)
    (= (plane-cost p6) 20)
    (unused p7)
    (= (goal-size p7) 8)
    (= (glaze-cost p7) 13)
    (= (grind-cost p7) 24)
    (= (plane-cost p7) 16)
    (= (board-size b0) 27)
    (wood b0 teak)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 40)
    (wood b1 mahogany)
    (surface-condition b1 rough)
    (available b1)
    (= (board-size b2) 8)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 mahogany)
          (surface-condition p0 smooth)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 mauve)
          (wood p1 mahogany)
          (surface-condition p1 smooth)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 mauve)
          (wood p2 mahogany)
          (surface-condition p2 smooth)
          (treatment p2 varnished)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 blue)
          (wood p3 teak)
          (surface-condition p3 verysmooth)
          (available p3)
      ))
      (preference g_p4_0 (and
          (colour p4 blue)
          (wood p4 mahogany)
          (treatment p4 glazed)
          (available p4)
      ))
      (preference g_p5_0 (and
          (colour p5 green)
          (wood p5 teak)
          (surface-condition p5 smooth)
          (treatment p5 glazed)
          (available p5)
      ))
      (preference g_p6_0 (and
          (colour p6 white)
          (wood p6 teak)
          (surface-condition p6 verysmooth)
          (treatment p6 glazed)
          (available p6)
      ))
      (preference g_p7_0 (and
          (colour p7 green)
          (wood p7 mahogany)
          (surface-condition p7 verysmooth)
          (treatment p7 glazed)
          (available p7)
      ))
    )
  )
  (:metric maximize
    (- 573
      (+ (total-cost)
         (* (is-violated g_p0_0) 59)
         (* (is-violated g_p1_0) 75)
         (* (is-violated g_p2_0) 91)
         (* (is-violated g_p3_0) 94)
         (* (is-violated g_p4_0) 49)
         (* (is-violated g_p5_0) 66)
         (* (is-violated g_p6_0) 65)
         (* (is-violated g_p7_0) 74)
  )))
)
