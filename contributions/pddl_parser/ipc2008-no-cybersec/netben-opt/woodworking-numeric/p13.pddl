; woodworking 'choose one' task with 5 parts
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 589938

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
    green blue mauve white - acolour
    cherry teak - awood
    p0 p1 p2 p3 p4 - part
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
    (has-colour glazer0 natural)
    (has-colour glazer0 green)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 green)
    (has-colour immersion-varnisher0 natural)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 green)
    (has-colour spray-varnisher0 natural)
    (unused p0)
    (= (goal-size p0) 12)
    (= (glaze-cost p0) 17)
    (= (grind-cost p0) 36)
    (= (plane-cost p0) 24)
    (unused p1)
    (= (goal-size p1) 5)
    (= (glaze-cost p1) 10)
    (= (grind-cost p1) 15)
    (= (plane-cost p1) 10)
    (unused p2)
    (= (goal-size p2) 9)
    (= (glaze-cost p2) 14)
    (= (grind-cost p2) 27)
    (= (plane-cost p2) 18)
    (unused p3)
    (= (goal-size p3) 7)
    (= (glaze-cost p3) 12)
    (= (grind-cost p3) 21)
    (= (plane-cost p3) 14)
    (unused p4)
    (= (goal-size p4) 11)
    (= (glaze-cost p4) 16)
    (= (grind-cost p4) 33)
    (= (plane-cost p4) 22)
    (= (board-size b0) 35)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 9)
    (wood b1 cherry)
    (surface-condition b1 smooth)
    (available b1)
    (= (board-size b2) 35)
    (wood b2 teak)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (colour p0 natural)
          (wood p0 cherry)
          (surface-condition p0 verysmooth)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p0_1 (and
          (wood p0 teak)
          (treatment p0 glazed)
          (available p0)
      ))
      (preference g_p0_2 (and
          (colour p0 blue)
          (surface-condition p0 smooth)
          (treatment p0 varnished)
          (available p0)
      ))
      (preference g_p1_0 (and
          (colour p1 mauve)
          (wood p1 cherry)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p1_1 (and
          (wood p1 teak)
          (treatment p1 glazed)
          (available p1)
      ))
      (preference g_p2_0 (and
          (wood p2 cherry)
          (surface-condition p2 smooth)
          (available p2)
      ))
      (preference g_p2_1 (and
          (wood p2 cherry)
          (surface-condition p2 verysmooth)
          (treatment p2 varnished)
          (available p2)
      ))
      (preference g_p2_2 (and
          (colour p2 green)
          (wood p2 cherry)
          (treatment p2 glazed)
          (available p2)
      ))
      (preference g_p3_0 (and
          (colour p3 green)
          (surface-condition p3 verysmooth)
          (available p3)
      ))
      (preference g_p3_1 (and
          (wood p3 teak)
          (surface-condition p3 smooth)
          (treatment p3 glazed)
          (available p3)
      ))
      (preference g_p3_2 (and
          (colour p3 natural)
          (surface-condition p3 smooth)
          (available p3)
      ))
      (preference g_p4_0 (and
          (surface-condition p4 smooth)
          (treatment p4 varnished)
          (available p4)
      ))
      (preference g_p4_1 (and
          (colour p4 mauve)
          (wood p4 teak)
          (surface-condition p4 verysmooth)
          (treatment p4 glazed)
          (available p4)
      ))
    )
  )
  (:metric maximize
    (- 1050
      (+ (total-cost)
         (* (is-violated g_p0_0) 122)
         (* (is-violated g_p0_1) 64)
         (* (is-violated g_p0_2) 112)
         (* (is-violated g_p1_0) 43)
         (* (is-violated g_p1_1) 46)
         (* (is-violated g_p2_0) 64)
         (* (is-violated g_p2_1) 86)
         (* (is-violated g_p2_2) 46)
         (* (is-violated g_p3_0) 76)
         (* (is-violated g_p3_1) 90)
         (* (is-violated g_p3_2) 87)
         (* (is-violated g_p4_0) 107)
         (* (is-violated g_p4_1) 107)
  )))
)
