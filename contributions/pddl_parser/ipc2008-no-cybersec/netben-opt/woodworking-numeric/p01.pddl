; woodworking 'sawing' task with 3 parts
; Machines:
;   1 highspeed-saw
;   1 saw
; random seed: 973895

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    saw0 - saw
    red black - acolour
    pine teak - awood
    p0 p1 p2 - part
    b0 b1 - board
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
    (= (goal-size p1) 9)
    (= (glaze-cost p1) 14)
    (= (grind-cost p1) 27)
    (= (plane-cost p1) 18)
    (unused p2)
    (= (goal-size p2) 11)
    (= (glaze-cost p2) 16)
    (= (grind-cost p2) 33)
    (= (plane-cost p2) 22)
    (= (board-size b0) 10)
    (wood b0 teak)
    (surface-condition b0 rough)
    (available b0)
    (= (board-size b1) 17)
    (wood b1 pine)
    (surface-condition b1 smooth)
    (available b1)
  )
  (:goal
    (and
      (preference g_p0_0 (and
          (wood p0 pine)
          (available p0)
      ))
      (preference g_p1_0 (and
          (wood p1 pine)
          (available p1)
      ))
      (preference g_p2_0 (available p2))
    )
  )
  (:metric maximize
    (- 112
      (+ (total-cost)
         (* (is-violated g_p0_0) 38)
         (* (is-violated g_p1_0) 43)
         (* (is-violated g_p2_0) 31)
  )))
)
