; woodworking 'not everything worthwhile' task with 7 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 257830

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
    blue green white red mauve - acolour
    cherry pine - awood
    p0 p1 p2 p3 p4 p5 p6 - part
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
    (has-colour glazer0 natural)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 green)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 green)
    (unused p0)
    (= (goal-size p0) 5)
    (= (glaze-cost p0) 10)
    (= (grind-cost p0) 15)
    (= (plane-cost p0) 10)
    (unused p1)
    (= (goal-size p1) 10)
    (= (glaze-cost p1) 15)
    (= (grind-cost p1) 30)
    (= (plane-cost p1) 20)
    (unused p2)
    (= (goal-size p2) 5)
    (= (glaze-cost p2) 10)
    (= (grind-cost p2) 15)
    (= (plane-cost p2) 10)
    (unused p3)
    (= (goal-size p3) 5)
    (= (glaze-cost p3) 10)
    (= (grind-cost p3) 15)
    (= (plane-cost p3) 10)
    (unused p4)
    (= (goal-size p4) 12)
    (= (glaze-cost p4) 17)
    (= (grind-cost p4) 36)
    (= (plane-cost p4) 24)
    (unused p5)
    (= (goal-size p5) 5)
    (= (glaze-cost p5) 10)
    (= (grind-cost p5) 15)
    (= (plane-cost p5) 10)
    (unused p6)
    (= (goal-size p6) 14)
    (= (glaze-cost p6) 19)
    (= (grind-cost p6) 42)
    (= (plane-cost p6) 28)
    (= (board-size b0) 27)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 29)
    (wood b1 pine)
    (surface-condition b1 rough)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 pine)
          (treatment p0 varnished)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 mauve)
          (wood p1 cherry)
          (surface-condition p1 smooth)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 green)
          (wood p2 pine)
          (treatment p2 varnished)
          (available p2)
      ))
      (preference g_p3_0 (and
          (wood p3 cherry)
          (surface-condition p3 verysmooth)
          (treatment p3 glazed)
          (available p3)
      ))
      (preference g_p4_0 (and
          (colour p4 green)
          (surface-condition p4 verysmooth)
          (treatment p4 glazed)
          (available p4)
      ))
      (preference g_p5_0 (and
          (wood p5 pine)
          (treatment p5 varnished)
          (available p5)
      ))
      (preference g_p6_0 (and
          (colour p6 natural)
          (wood p6 pine)
          (surface-condition p6 verysmooth)
          (treatment p6 glazed)
          (available p6)
      ))
    )
  )
  (:metric maximize
    (- 483
      (+ (total-cost)
         (* (is-violated g_p0_0) 38)
         (* (is-violated g_p1_0) 75)
         (* (is-violated g_p2_0) 39)
         (* (is-violated g_p3_0) 65)
         (* (is-violated g_p4_0) 92)
         (* (is-violated g_p5_0) 39)
         (* (is-violated g_p6_0) 135)
  )))
)
