; woodworking 'sawing' task with 7 parts
; Machines:
;   1 highspeed-saw
;   1 saw
; random seed: 743580

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    saw0 - saw
    black red mauve green blue - acolour
    walnut beech - awood
    p0 p1 p2 p3 p4 p5 p6 - part
    b0 b1 b2 - board
  )
  (:init
    (= (total-cost) 0)
    (empty highspeed-saw0)
    (unused p0)
    (= (goal-size p0) 5)
    (= (glaze-cost p0) 10)
    (= (grind-cost p0) 15)
    (= (plane-cost p0) 10)
    (unused p1)
    (= (goal-size p1) 14)
    (= (glaze-cost p1) 19)
    (= (grind-cost p1) 42)
    (= (plane-cost p1) 28)
    (unused p2)
    (= (goal-size p2) 7)
    (= (glaze-cost p2) 12)
    (= (grind-cost p2) 21)
    (= (plane-cost p2) 14)
    (unused p3)
    (= (goal-size p3) 8)
    (= (glaze-cost p3) 13)
    (= (grind-cost p3) 24)
    (= (plane-cost p3) 16)
    (unused p4)
    (= (goal-size p4) 11)
    (= (glaze-cost p4) 16)
    (= (grind-cost p4) 33)
    (= (plane-cost p4) 22)
    (unused p5)
    (= (goal-size p5) 12)
    (= (glaze-cost p5) 17)
    (= (grind-cost p5) 36)
    (= (plane-cost p5) 24)
    (unused p6)
    (= (goal-size p6) 12)
    (= (glaze-cost p6) 17)
    (= (grind-cost p6) 36)
    (= (plane-cost p6) 24)
    (= (board-size b0) 14)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 38)
    (wood b1 walnut)
    (surface-condition b1 rough)
    (available b1)
    (= (board-size b2) 11)
    (wood b2 walnut)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (available p0))
      (preference g_p1_0 (available p1))
      (preference g_p2_0 (and
          (wood p2 beech)
          (available p2)
      ))
      (preference g_p3_0 (and
          (wood p3 beech)
          (available p3)
      ))
      (preference g_p4_0 (and
          (wood p4 walnut)
          (available p4)
      ))
      (preference g_p5_0 (and
          (wood p5 walnut)
          (available p5)
      ))
      (preference g_p6_0 (available p6))
    )
  )
  (:metric maximize
    (- 280
      (+ (total-cost)
         (* (is-violated g_p0_0) 40)
         (* (is-violated g_p1_0) 38)
         (* (is-violated g_p2_0) 49)
         (* (is-violated g_p3_0) 33)
         (* (is-violated g_p4_0) 46)
         (* (is-violated g_p5_0) 48)
         (* (is-violated g_p6_0) 26)
  )))
)
