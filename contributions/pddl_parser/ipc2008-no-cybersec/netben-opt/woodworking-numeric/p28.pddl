; woodworking 'not everything worthwhile' task with 10 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 994072

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
    red white black blue green mauve - acolour
    mahogany beech teak - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 - part
    b0 b1 b2 b3 - board
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
    (has-colour glazer0 white)
    (has-colour glazer0 natural)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 green)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 green)
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
    (= (goal-size p2) 8)
    (= (glaze-cost p2) 13)
    (= (grind-cost p2) 24)
    (= (plane-cost p2) 16)
    (unused p3)
    (= (goal-size p3) 7)
    (= (glaze-cost p3) 12)
    (= (grind-cost p3) 21)
    (= (plane-cost p3) 14)
    (unused p4)
    (= (goal-size p4) 6)
    (= (glaze-cost p4) 11)
    (= (grind-cost p4) 18)
    (= (plane-cost p4) 12)
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
    (= (goal-size p7) 6)
    (= (glaze-cost p7) 11)
    (= (grind-cost p7) 18)
    (= (plane-cost p7) 12)
    (unused p8)
    (= (goal-size p8) 15)
    (= (glaze-cost p8) 20)
    (= (grind-cost p8) 45)
    (= (plane-cost p8) 30)
    (unused p9)
    (= (goal-size p9) 6)
    (= (glaze-cost p9) 11)
    (= (grind-cost p9) 18)
    (= (plane-cost p9) 12)
    (= (board-size b0) 23)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 34)
    (wood b1 teak)
    (surface-condition b1 rough)
    (available b1)
    (= (board-size b2) 15)
    (wood b2 teak)
    (surface-condition b2 smooth)
    (available b2)
    (= (board-size b3) 13)
    (wood b3 mahogany)
    (surface-condition b3 rough)
    (available b3)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 mahogany)
          (surface-condition p0 smooth)
          (available p0)
      ))
      (preference g_p1_0 (and
          (wood p1 teak)
          (surface-condition p1 verysmooth)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p2_0 (and
          (wood p2 teak)
          (treatment p2 glazed)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 white)
          (surface-condition p3 smooth)
          (treatment p3 varnished)
          (available p3)
      ))
      (preference g_p4_0 (and
          (wood p4 mahogany)
          (treatment p4 glazed)
          (available p4)
      ))
      (preference g_p5_0 (and
          (colour p5 green)
          (surface-condition p5 verysmooth)
          (available p5)
      ))
      (preference g_p6_0 (and
          (colour p6 white)
          (wood p6 beech)
          (available p6)
      ))
      (preference g_p7_0 (and
          (wood p7 teak)
          (surface-condition p7 smooth)
          (available p7)
      ))
      (preference g_p8_0 (and
          (colour p8 natural)
          (wood p8 teak)
          (treatment p8 glazed)
          (available p8)
      ))
      (preference g_p9_0 (and
          (wood p9 beech)
          (treatment p9 varnished)
          (available p9)
      ))
    )
  )
  (:metric maximize
    (- 544
      (+ (total-cost)
         (* (is-violated g_p0_0) 50)
         (* (is-violated g_p1_0) 75)
         (* (is-violated g_p2_0) 44)
         (* (is-violated g_p3_0) 60)
         (* (is-violated g_p4_0) 33)
         (* (is-violated g_p5_0) 80)
         (* (is-violated g_p6_0) 49)
         (* (is-violated g_p7_0) 69)
         (* (is-violated g_p8_0) 50)
         (* (is-violated g_p9_0) 34)
  )))
)
