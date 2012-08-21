; woodworking 'not everything worthwhile' task with 5 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 605112

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
    blue red green black - acolour
    beech pine - awood
    p0 p1 p2 p3 p4 - part
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
    (has-colour glazer0 natural)
    (has-colour glazer0 green)
    (has-colour glazer0 black)
    (has-colour immersion-varnisher0 black)
    (has-colour immersion-varnisher0 green)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 black)
    (has-colour spray-varnisher0 green)
    (unused p0)
    (= (goal-size p0) 5)
    (= (glaze-cost p0) 10)
    (= (grind-cost p0) 15)
    (= (plane-cost p0) 10)
    (unused p1)
    (= (goal-size p1) 12)
    (= (glaze-cost p1) 17)
    (= (grind-cost p1) 36)
    (= (plane-cost p1) 24)
    (unused p2)
    (= (goal-size p2) 9)
    (= (glaze-cost p2) 14)
    (= (grind-cost p2) 27)
    (= (plane-cost p2) 18)
    (unused p3)
    (= (goal-size p3) 6)
    (= (glaze-cost p3) 11)
    (= (grind-cost p3) 18)
    (= (plane-cost p3) 12)
    (unused p4)
    (= (goal-size p4) 11)
    (= (glaze-cost p4) 16)
    (= (grind-cost p4) 33)
    (= (plane-cost p4) 22)
    (= (board-size b0) 11)
    (wood b0 beech)
    (surface-condition b0 smooth)
    (available b0)
    (= (board-size b1) 32)
    (wood b1 pine)
    (surface-condition b1 smooth)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 beech)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 natural)
          (wood p1 pine)
          (surface-condition p1 verysmooth)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 black)
          (surface-condition p2 smooth)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 green)
          (wood p3 beech)
          (surface-condition p3 smooth)
          (treatment p3 glazed)
          (available p3)
      ))
      (preference g_p4_0 (and
          (colour p4 green)
          (wood p4 pine)
          (surface-condition p4 verysmooth)
          (treatment p4 varnished)
          (available p4)
      ))
    )
  )
  (:metric maximize
    (- 379
      (+ (total-cost)
         (* (is-violated g_p0_0) 40)
         (* (is-violated g_p1_0) 116)
         (* (is-violated g_p2_0) 92)
         (* (is-violated g_p3_0) 67)
         (* (is-violated g_p4_0) 64)
  )))
)
