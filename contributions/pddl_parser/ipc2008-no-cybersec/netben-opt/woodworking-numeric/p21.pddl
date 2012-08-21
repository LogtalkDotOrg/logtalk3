; woodworking 'not everything worthwhile' task with 3 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 489150

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
    white mauve - acolour
    mahogany walnut - awood
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
    (has-colour glazer0 natural)
    (has-colour immersion-varnisher0 white)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 white)
    (unused p0)
    (= (goal-size p0) 13)
    (= (glaze-cost p0) 18)
    (= (grind-cost p0) 39)
    (= (plane-cost p0) 26)
    (unused p1)
    (= (goal-size p1) 8)
    (= (glaze-cost p1) 13)
    (= (grind-cost p1) 24)
    (= (plane-cost p1) 16)
    (unused p2)
    (= (goal-size p2) 14)
    (= (glaze-cost p2) 19)
    (= (grind-cost p2) 42)
    (= (plane-cost p2) 28)
    (= (board-size b0) 8)
    (wood b0 mahogany)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 27)
    (wood b1 walnut)
    (surface-condition b1 rough)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (colour p0 natural)
          (wood p0 walnut)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 white)
          (wood p1 mahogany)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p2_0 (and
          (colour p2 white)
          (wood p2 walnut)
          (surface-condition p2 verysmooth)
          (treatment p2 varnished)
          (available p2)
      ))
    )
  )
  (:metric maximize
    (- 195
      (+ (total-cost)
         (* (is-violated g_p0_0) 48)
         (* (is-violated g_p1_0) 32)
         (* (is-violated g_p2_0) 115)
  )))
)
