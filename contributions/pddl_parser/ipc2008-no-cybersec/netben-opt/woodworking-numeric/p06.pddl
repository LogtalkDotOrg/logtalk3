; woodworking 'sawing' task with 8 parts
; Machines:
;   1 highspeed-saw
;   1 saw
; random seed: 159992

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    saw0 - saw
    green mauve white blue red black - acolour
    beech walnut - awood
    p0 p1 p2 p3 p4 p5 p6 p7 - part
    b0 b1 b2 - board
  )
  (:init
    (= (total-cost) 0)
    (empty highspeed-saw0)
    (unused p0)
    (= (goal-size p0) 9)
    (= (glaze-cost p0) 14)
    (= (grind-cost p0) 27)
    (= (plane-cost p0) 18)
    (unused p1)
    (= (goal-size p1) 8)
    (= (glaze-cost p1) 13)
    (= (grind-cost p1) 24)
    (= (plane-cost p1) 16)
    (unused p2)
    (= (goal-size p2) 12)
    (= (glaze-cost p2) 17)
    (= (grind-cost p2) 36)
    (= (plane-cost p2) 24)
    (unused p3)
    (= (goal-size p3) 14)
    (= (glaze-cost p3) 19)
    (= (grind-cost p3) 42)
    (= (plane-cost p3) 28)
    (unused p4)
    (= (goal-size p4) 10)
    (= (glaze-cost p4) 15)
    (= (grind-cost p4) 30)
    (= (plane-cost p4) 20)
    (unused p5)
    (= (goal-size p5) 14)
    (= (glaze-cost p5) 19)
    (= (grind-cost p5) 42)
    (= (plane-cost p5) 28)
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
    (= (board-size b0) 27)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 40)
    (wood b1 walnut)
    (surface-condition b1 smooth)
    (available b1)
    (= (board-size b2) 9)
    (wood b2 walnut)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 beech)
          (available p0)
      ))
      (preference g_p1_0 (available p1))
      (preference g_p2_0 (available p2))
      (preference g_p3_0 (available p3))
      (preference g_p4_0 (and
          (wood p4 walnut)
          (available p4)
      ))
      (preference g_p5_0 (and
          (wood p5 beech)
          (available p5)
      ))
      (preference g_p6_0 (available p6))
      (preference g_p7_0 (and
          (wood p7 beech)
          (available p7)
      ))
    )
  )
  (:metric maximize
    (- 301
      (+ (total-cost)
         (* (is-violated g_p0_0) 34)
         (* (is-violated g_p1_0) 29)
         (* (is-violated g_p2_0) 28)
         (* (is-violated g_p3_0) 39)
         (* (is-violated g_p4_0) 36)
         (* (is-violated g_p5_0) 49)
         (* (is-violated g_p6_0) 39)
         (* (is-violated g_p7_0) 47)
  )))
)
