; woodworking 'choose one' task with 4 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 861437

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
    mauve red white - acolour
    walnut oak - awood
    p0 p1 p2 p3 - part
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
    (has-colour glazer0 white)
    (has-colour glazer0 natural)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (= (goal-size p0) 15)
    (= (glaze-cost p0) 20)
    (= (grind-cost p0) 45)
    (= (plane-cost p0) 30)
    (unused p1)
    (= (goal-size p1) 6)
    (= (glaze-cost p1) 11)
    (= (grind-cost p1) 18)
    (= (plane-cost p1) 12)
    (unused p2)
    (= (goal-size p2) 13)
    (= (glaze-cost p2) 18)
    (= (grind-cost p2) 39)
    (= (plane-cost p2) 26)
    (unused p3)
    (= (goal-size p3) 15)
    (= (glaze-cost p3) 20)
    (= (grind-cost p3) 45)
    (= (plane-cost p3) 30)
    (= (board-size b0) 38)
    (wood b0 oak)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 45)
    (wood b1 oak)
    (surface-condition b1 rough)
    (available b1)
    (= (board-size b2) 27)
    (wood b2 walnut)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 walnut)
          (treatment p0 varnished)
          (available p0)
      ))
      (preference g_p0_1 (and
          (surface-condition p0 smooth)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p1_0 (and
          (wood p1 oak)
          (surface-condition p1 smooth)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p1_1 (and
          (colour p1 mauve)
          (wood p1 oak)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p1_2 (and
          (colour p1 mauve)
          (wood p1 walnut)
          (surface-condition p1 verysmooth)
          (treatment p1 varnished)
          (available p1)
      ))
      (preference g_p1_3 (and
          (colour p1 red)
          (wood p1 walnut)
          (surface-condition p1 verysmooth)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p2_0 (and
          (wood p2 oak)
          (treatment p2 glazed)
          (available p2)
      ))
      (preference g_p2_1 (and
          (colour p2 white)
          (surface-condition p2 smooth)
          (available p2)
      ))
      (preference g_p2_2 (and
          (wood p2 oak)
          (surface-condition p2 verysmooth)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 red)
          (wood p3 oak)
          (surface-condition p3 verysmooth)
          (treatment p3 varnished)
          (available p3)
      ))
      (preference g_p3_1 (and
          (colour p3 red)
          (wood p3 oak)
          (surface-condition p3 smooth)
          (available p3)
      ))
      (preference g_p3_2 (and
          (colour p3 natural)
          (wood p3 oak)
          (surface-condition p3 smooth)
          (treatment p3 glazed)
          (available p3)
      ))
      (preference g_p3_3 (and
          (colour p3 red)
          (treatment p3 glazed)
          (available p3)
      ))
    )
  )
  (:metric maximize
    (- 1130
      (+ (total-cost)
         (* (is-violated g_p0_0) 63)
         (* (is-violated g_p0_1) 124)
         (* (is-violated g_p1_0) 83)
         (* (is-violated g_p1_1) 48)
         (* (is-violated g_p1_2) 77)
         (* (is-violated g_p1_3) 70)
         (* (is-violated g_p2_0) 66)
         (* (is-violated g_p2_1) 108)
         (* (is-violated g_p2_2) 88)
         (* (is-violated g_p3_0) 114)
         (* (is-violated g_p3_1) 101)
         (* (is-violated g_p3_2) 124)
         (* (is-violated g_p3_3) 64)
  )))
)
