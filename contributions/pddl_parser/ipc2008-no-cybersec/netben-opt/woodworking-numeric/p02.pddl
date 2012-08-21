; woodworking 'sawing' task with 4 parts
; Machines:
;   1 highspeed-saw
;   1 saw
; random seed: 760827

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    saw0 - saw
    blue green mauve - acolour
    oak teak - awood
    p0 p1 p2 p3 - part
    b0 b1 - board
  )
  (:init
    (= (total-cost) 0)
    (empty highspeed-saw0)
    (unused p0)
    (= (goal-size p0) 7)
    (= (glaze-cost p0) 12)
    (= (grind-cost p0) 21)
    (= (plane-cost p0) 14)
    (unused p1)
    (= (goal-size p1) 12)
    (= (glaze-cost p1) 17)
    (= (grind-cost p1) 36)
    (= (plane-cost p1) 24)
    (unused p2)
    (= (goal-size p2) 12)
    (= (glaze-cost p2) 17)
    (= (grind-cost p2) 36)
    (= (plane-cost p2) 24)
    (unused p3)
    (= (goal-size p3) 12)
    (= (glaze-cost p3) 17)
    (= (grind-cost p3) 36)
    (= (plane-cost p3) 24)
    (= (board-size b0) 11)
    (wood b0 teak)
    (surface-condition b0 smooth)
    (available b0)
    (= (board-size b1) 28)
    (wood b1 oak)
    (surface-condition b1 rough)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (available p0))
      (preference g_p1_0 (available p1))
      (preference g_p2_0 (available p2))
      (preference g_p3_0 (available p3))
    )
  )
  (:metric maximize
    (- 141
      (+ (total-cost)
         (* (is-violated g_p0_0) 38)
         (* (is-violated g_p1_0) 35)
         (* (is-violated g_p2_0) 29)
         (* (is-violated g_p3_0) 39)
  )))
)
