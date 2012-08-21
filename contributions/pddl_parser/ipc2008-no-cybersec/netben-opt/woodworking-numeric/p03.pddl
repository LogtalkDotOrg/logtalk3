; woodworking 'sawing' task with 5 parts
; Machines:
;   1 highspeed-saw
;   1 saw
; random seed: 334053

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    saw0 - saw
    red blue green mauve - acolour
    beech cherry - awood
    p0 p1 p2 p3 p4 - part
    b0 b1 - board
  )
  (:init
    (= (total-cost) 0)
    (empty highspeed-saw0)
    (unused p0)
    (= (goal-size p0) 6)
    (= (glaze-cost p0) 11)
    (= (grind-cost p0) 18)
    (= (plane-cost p0) 12)
    (unused p1)
    (= (goal-size p1) 12)
    (= (glaze-cost p1) 17)
    (= (grind-cost p1) 36)
    (= (plane-cost p1) 24)
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
    (unused p4)
    (= (goal-size p4) 10)
    (= (glaze-cost p4) 15)
    (= (grind-cost p4) 30)
    (= (plane-cost p4) 20)
    (= (board-size b0) 10)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 35)
    (wood b1 cherry)
    (surface-condition b1 rough)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (available p0))
      (preference g_p1_0 (and
          (wood p1 cherry)
          (available p1)
      ))
      (preference g_p2_0 (and
          (wood p2 cherry)
          (available p2)
      ))
      (preference g_p3_0 (available p3))
      (preference g_p4_0 (and
          (wood p4 cherry)
          (available p4)
      ))
    )
  )
  (:metric maximize
    (- 184
      (+ (total-cost)
         (* (is-violated g_p0_0) 31)
         (* (is-violated g_p1_0) 39)
         (* (is-violated g_p2_0) 42)
         (* (is-violated g_p3_0) 37)
         (* (is-violated g_p4_0) 35)
  )))
)
