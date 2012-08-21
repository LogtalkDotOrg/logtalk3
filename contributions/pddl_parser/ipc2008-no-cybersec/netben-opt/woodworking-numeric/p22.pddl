; woodworking 'not everything worthwhile' task with 4 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 860712

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
    red mauve green - acolour
    pine beech - awood
    p0 p1 p2 p3 - part
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
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 natural)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 natural)
    (unused p0)
    (= (goal-size p0) 6)
    (= (glaze-cost p0) 11)
    (= (grind-cost p0) 18)
    (= (plane-cost p0) 12)
    (unused p1)
    (= (goal-size p1) 8)
    (= (glaze-cost p1) 13)
    (= (grind-cost p1) 24)
    (= (plane-cost p1) 16)
    (unused p2)
    (= (goal-size p2) 10)
    (= (glaze-cost p2) 15)
    (= (grind-cost p2) 30)
    (= (plane-cost p2) 20)
    (unused p3)
    (= (goal-size p3) 11)
    (= (glaze-cost p3) 16)
    (= (grind-cost p3) 33)
    (= (plane-cost p3) 22)
    (= (board-size b0) 8)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 27)
    (wood b1 pine)
    (surface-condition b1 rough)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (colour p0 mauve)
          (surface-condition p0 smooth)
          (treatment p0 varnished)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 natural)
          (wood p1 beech)
          (surface-condition p1 verysmooth)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 mauve)
          (wood p2 pine)
          (treatment p2 varnished)
          (available p2)
      ))
      (preference g_p3_0 (and
          (wood p3 pine)
          (surface-condition p3 smooth)
          (available p3)
      ))
    )
  )
  (:metric maximize
    (- 278
      (+ (total-cost)
         (* (is-violated g_p0_0) 73)
         (* (is-violated g_p1_0) 76)
         (* (is-violated g_p2_0) 43)
         (* (is-violated g_p3_0) 86)
  )))
)
