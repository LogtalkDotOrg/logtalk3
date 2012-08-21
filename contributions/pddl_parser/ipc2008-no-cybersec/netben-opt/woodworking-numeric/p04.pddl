; woodworking 'sawing' task with 6 parts
; Machines:
;   1 highspeed-saw
;   1 saw
; random seed: 544269

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    saw0 - saw
    white red blue green - acolour
    beech pine - awood
    p0 p1 p2 p3 p4 p5 - part
    b0 b1 b2 - board
  )
  (:init
    (= (total-cost) 0)
    (empty highspeed-saw0)
    (unused p0)
    (= (goal-size p0) 10)
    (= (glaze-cost p0) 15)
    (= (grind-cost p0) 30)
    (= (plane-cost p0) 20)
    (unused p1)
    (= (goal-size p1) 11)
    (= (glaze-cost p1) 16)
    (= (grind-cost p1) 33)
    (= (plane-cost p1) 22)
    (unused p2)
    (= (goal-size p2) 6)
    (= (glaze-cost p2) 11)
    (= (grind-cost p2) 18)
    (= (plane-cost p2) 12)
    (unused p3)
    (= (goal-size p3) 5)
    (= (glaze-cost p3) 10)
    (= (grind-cost p3) 15)
    (= (plane-cost p3) 10)
    (unused p4)
    (= (goal-size p4) 6)
    (= (glaze-cost p4) 11)
    (= (grind-cost p4) 18)
    (= (plane-cost p4) 12)
    (unused p5)
    (= (goal-size p5) 6)
    (= (glaze-cost p5) 11)
    (= (grind-cost p5) 18)
    (= (plane-cost p5) 12)
    (= (board-size b0) 6)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 29)
    (wood b1 pine)
    (surface-condition b1 rough)
    (available b1)
    (= (board-size b2) 6)
    (wood b2 pine)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 pine)
          (available p0)
      ))
      (preference g_p1_0 (available p1))
      (preference g_p2_0 (and
          (wood p2 pine)
          (available p2)
      ))
      (preference g_p3_0 (and
          (wood p3 pine)
          (available p3)
      ))
      (preference g_p4_0 (available p4))
      (preference g_p5_0 (available p5))
    )
  )
  (:metric maximize
    (- 228
      (+ (total-cost)
         (* (is-violated g_p0_0) 48)
         (* (is-violated g_p1_0) 30)
         (* (is-violated g_p2_0) 40)
         (* (is-violated g_p3_0) 40)
         (* (is-violated g_p4_0) 35)
         (* (is-violated g_p5_0) 35)
  )))
)
