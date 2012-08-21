;; ######
;; #    ##
;; # $ $ ##
;; ## $$  #
;;  # #   #
;;  # ## ##
;;  #  . .#
;;  # @. .#
;;  #  ####
;;  ####

(define (problem p083-microban-sequential)
  (:domain sokoban-sequential)
  (:objects
    dir-down - direction
    dir-left - direction
    dir-right - direction
    dir-up - direction
    player-01 - player
    pos-01-01 - location
    pos-01-02 - location
    pos-01-03 - location
    pos-01-04 - location
    pos-01-05 - location
    pos-01-06 - location
    pos-01-07 - location
    pos-01-08 - location
    pos-01-09 - location
    pos-01-10 - location
    pos-02-01 - location
    pos-02-02 - location
    pos-02-03 - location
    pos-02-04 - location
    pos-02-05 - location
    pos-02-06 - location
    pos-02-07 - location
    pos-02-08 - location
    pos-02-09 - location
    pos-02-10 - location
    pos-03-01 - location
    pos-03-02 - location
    pos-03-03 - location
    pos-03-04 - location
    pos-03-05 - location
    pos-03-06 - location
    pos-03-07 - location
    pos-03-08 - location
    pos-03-09 - location
    pos-03-10 - location
    pos-04-01 - location
    pos-04-02 - location
    pos-04-03 - location
    pos-04-04 - location
    pos-04-05 - location
    pos-04-06 - location
    pos-04-07 - location
    pos-04-08 - location
    pos-04-09 - location
    pos-04-10 - location
    pos-05-01 - location
    pos-05-02 - location
    pos-05-03 - location
    pos-05-04 - location
    pos-05-05 - location
    pos-05-06 - location
    pos-05-07 - location
    pos-05-08 - location
    pos-05-09 - location
    pos-05-10 - location
    pos-06-01 - location
    pos-06-02 - location
    pos-06-03 - location
    pos-06-04 - location
    pos-06-05 - location
    pos-06-06 - location
    pos-06-07 - location
    pos-06-08 - location
    pos-06-09 - location
    pos-06-10 - location
    pos-07-01 - location
    pos-07-02 - location
    pos-07-03 - location
    pos-07-04 - location
    pos-07-05 - location
    pos-07-06 - location
    pos-07-07 - location
    pos-07-08 - location
    pos-07-09 - location
    pos-07-10 - location
    pos-08-01 - location
    pos-08-02 - location
    pos-08-03 - location
    pos-08-04 - location
    pos-08-05 - location
    pos-08-06 - location
    pos-08-07 - location
    pos-08-08 - location
    pos-08-09 - location
    pos-08-10 - location
    stone-01 - stone
    stone-02 - stone
    stone-03 - stone
    stone-04 - stone
  )
  (:init
    (IS-GOAL pos-05-07)
    (IS-GOAL pos-05-08)
    (IS-GOAL pos-07-07)
    (IS-GOAL pos-07-08)
    (IS-NONGOAL pos-01-01)
    (IS-NONGOAL pos-01-02)
    (IS-NONGOAL pos-01-03)
    (IS-NONGOAL pos-01-04)
    (IS-NONGOAL pos-01-05)
    (IS-NONGOAL pos-01-06)
    (IS-NONGOAL pos-01-07)
    (IS-NONGOAL pos-01-08)
    (IS-NONGOAL pos-01-09)
    (IS-NONGOAL pos-01-10)
    (IS-NONGOAL pos-02-01)
    (IS-NONGOAL pos-02-02)
    (IS-NONGOAL pos-02-03)
    (IS-NONGOAL pos-02-04)
    (IS-NONGOAL pos-02-05)
    (IS-NONGOAL pos-02-06)
    (IS-NONGOAL pos-02-07)
    (IS-NONGOAL pos-02-08)
    (IS-NONGOAL pos-02-09)
    (IS-NONGOAL pos-02-10)
    (IS-NONGOAL pos-03-01)
    (IS-NONGOAL pos-03-02)
    (IS-NONGOAL pos-03-03)
    (IS-NONGOAL pos-03-04)
    (IS-NONGOAL pos-03-05)
    (IS-NONGOAL pos-03-06)
    (IS-NONGOAL pos-03-07)
    (IS-NONGOAL pos-03-08)
    (IS-NONGOAL pos-03-09)
    (IS-NONGOAL pos-03-10)
    (IS-NONGOAL pos-04-01)
    (IS-NONGOAL pos-04-02)
    (IS-NONGOAL pos-04-03)
    (IS-NONGOAL pos-04-04)
    (IS-NONGOAL pos-04-05)
    (IS-NONGOAL pos-04-06)
    (IS-NONGOAL pos-04-07)
    (IS-NONGOAL pos-04-08)
    (IS-NONGOAL pos-04-09)
    (IS-NONGOAL pos-04-10)
    (IS-NONGOAL pos-05-01)
    (IS-NONGOAL pos-05-02)
    (IS-NONGOAL pos-05-03)
    (IS-NONGOAL pos-05-04)
    (IS-NONGOAL pos-05-05)
    (IS-NONGOAL pos-05-06)
    (IS-NONGOAL pos-05-09)
    (IS-NONGOAL pos-05-10)
    (IS-NONGOAL pos-06-01)
    (IS-NONGOAL pos-06-02)
    (IS-NONGOAL pos-06-03)
    (IS-NONGOAL pos-06-04)
    (IS-NONGOAL pos-06-05)
    (IS-NONGOAL pos-06-06)
    (IS-NONGOAL pos-06-07)
    (IS-NONGOAL pos-06-08)
    (IS-NONGOAL pos-06-09)
    (IS-NONGOAL pos-06-10)
    (IS-NONGOAL pos-07-01)
    (IS-NONGOAL pos-07-02)
    (IS-NONGOAL pos-07-03)
    (IS-NONGOAL pos-07-04)
    (IS-NONGOAL pos-07-05)
    (IS-NONGOAL pos-07-06)
    (IS-NONGOAL pos-07-09)
    (IS-NONGOAL pos-07-10)
    (IS-NONGOAL pos-08-01)
    (IS-NONGOAL pos-08-02)
    (IS-NONGOAL pos-08-03)
    (IS-NONGOAL pos-08-04)
    (IS-NONGOAL pos-08-05)
    (IS-NONGOAL pos-08-06)
    (IS-NONGOAL pos-08-07)
    (IS-NONGOAL pos-08-08)
    (IS-NONGOAL pos-08-09)
    (IS-NONGOAL pos-08-10)
    (MOVE-DIR pos-01-05 pos-01-06 dir-down)
    (MOVE-DIR pos-01-06 pos-01-05 dir-up)
    (MOVE-DIR pos-01-06 pos-01-07 dir-down)
    (MOVE-DIR pos-01-07 pos-01-06 dir-up)
    (MOVE-DIR pos-01-07 pos-01-08 dir-down)
    (MOVE-DIR pos-01-08 pos-01-07 dir-up)
    (MOVE-DIR pos-01-08 pos-01-09 dir-down)
    (MOVE-DIR pos-01-09 pos-01-08 dir-up)
    (MOVE-DIR pos-01-09 pos-01-10 dir-down)
    (MOVE-DIR pos-01-10 pos-01-09 dir-up)
    (MOVE-DIR pos-02-02 pos-02-03 dir-down)
    (MOVE-DIR pos-02-02 pos-03-02 dir-right)
    (MOVE-DIR pos-02-03 pos-02-02 dir-up)
    (MOVE-DIR pos-02-03 pos-03-03 dir-right)
    (MOVE-DIR pos-03-02 pos-02-02 dir-left)
    (MOVE-DIR pos-03-02 pos-03-03 dir-down)
    (MOVE-DIR pos-03-02 pos-04-02 dir-right)
    (MOVE-DIR pos-03-03 pos-02-03 dir-left)
    (MOVE-DIR pos-03-03 pos-03-02 dir-up)
    (MOVE-DIR pos-03-03 pos-03-04 dir-down)
    (MOVE-DIR pos-03-03 pos-04-03 dir-right)
    (MOVE-DIR pos-03-04 pos-03-03 dir-up)
    (MOVE-DIR pos-03-04 pos-03-05 dir-down)
    (MOVE-DIR pos-03-04 pos-04-04 dir-right)
    (MOVE-DIR pos-03-05 pos-03-04 dir-up)
    (MOVE-DIR pos-03-05 pos-03-06 dir-down)
    (MOVE-DIR pos-03-06 pos-03-05 dir-up)
    (MOVE-DIR pos-03-06 pos-03-07 dir-down)
    (MOVE-DIR pos-03-07 pos-03-06 dir-up)
    (MOVE-DIR pos-03-07 pos-03-08 dir-down)
    (MOVE-DIR pos-03-07 pos-04-07 dir-right)
    (MOVE-DIR pos-03-08 pos-03-07 dir-up)
    (MOVE-DIR pos-03-08 pos-03-09 dir-down)
    (MOVE-DIR pos-03-08 pos-04-08 dir-right)
    (MOVE-DIR pos-03-09 pos-03-08 dir-up)
    (MOVE-DIR pos-03-09 pos-04-09 dir-right)
    (MOVE-DIR pos-04-02 pos-03-02 dir-left)
    (MOVE-DIR pos-04-02 pos-04-03 dir-down)
    (MOVE-DIR pos-04-02 pos-05-02 dir-right)
    (MOVE-DIR pos-04-03 pos-03-03 dir-left)
    (MOVE-DIR pos-04-03 pos-04-02 dir-up)
    (MOVE-DIR pos-04-03 pos-04-04 dir-down)
    (MOVE-DIR pos-04-03 pos-05-03 dir-right)
    (MOVE-DIR pos-04-04 pos-03-04 dir-left)
    (MOVE-DIR pos-04-04 pos-04-03 dir-up)
    (MOVE-DIR pos-04-04 pos-05-04 dir-right)
    (MOVE-DIR pos-04-07 pos-03-07 dir-left)
    (MOVE-DIR pos-04-07 pos-04-08 dir-down)
    (MOVE-DIR pos-04-07 pos-05-07 dir-right)
    (MOVE-DIR pos-04-08 pos-03-08 dir-left)
    (MOVE-DIR pos-04-08 pos-04-07 dir-up)
    (MOVE-DIR pos-04-08 pos-04-09 dir-down)
    (MOVE-DIR pos-04-08 pos-05-08 dir-right)
    (MOVE-DIR pos-04-09 pos-03-09 dir-left)
    (MOVE-DIR pos-04-09 pos-04-08 dir-up)
    (MOVE-DIR pos-05-02 pos-04-02 dir-left)
    (MOVE-DIR pos-05-02 pos-05-03 dir-down)
    (MOVE-DIR pos-05-03 pos-04-03 dir-left)
    (MOVE-DIR pos-05-03 pos-05-02 dir-up)
    (MOVE-DIR pos-05-03 pos-05-04 dir-down)
    (MOVE-DIR pos-05-03 pos-06-03 dir-right)
    (MOVE-DIR pos-05-04 pos-04-04 dir-left)
    (MOVE-DIR pos-05-04 pos-05-03 dir-up)
    (MOVE-DIR pos-05-04 pos-05-05 dir-down)
    (MOVE-DIR pos-05-04 pos-06-04 dir-right)
    (MOVE-DIR pos-05-05 pos-05-04 dir-up)
    (MOVE-DIR pos-05-05 pos-06-05 dir-right)
    (MOVE-DIR pos-05-07 pos-04-07 dir-left)
    (MOVE-DIR pos-05-07 pos-05-08 dir-down)
    (MOVE-DIR pos-05-07 pos-06-07 dir-right)
    (MOVE-DIR pos-05-08 pos-04-08 dir-left)
    (MOVE-DIR pos-05-08 pos-05-07 dir-up)
    (MOVE-DIR pos-05-08 pos-06-08 dir-right)
    (MOVE-DIR pos-06-03 pos-05-03 dir-left)
    (MOVE-DIR pos-06-03 pos-06-04 dir-down)
    (MOVE-DIR pos-06-04 pos-05-04 dir-left)
    (MOVE-DIR pos-06-04 pos-06-03 dir-up)
    (MOVE-DIR pos-06-04 pos-06-05 dir-down)
    (MOVE-DIR pos-06-04 pos-07-04 dir-right)
    (MOVE-DIR pos-06-05 pos-05-05 dir-left)
    (MOVE-DIR pos-06-05 pos-06-04 dir-up)
    (MOVE-DIR pos-06-05 pos-06-06 dir-down)
    (MOVE-DIR pos-06-05 pos-07-05 dir-right)
    (MOVE-DIR pos-06-06 pos-06-05 dir-up)
    (MOVE-DIR pos-06-06 pos-06-07 dir-down)
    (MOVE-DIR pos-06-07 pos-05-07 dir-left)
    (MOVE-DIR pos-06-07 pos-06-06 dir-up)
    (MOVE-DIR pos-06-07 pos-06-08 dir-down)
    (MOVE-DIR pos-06-07 pos-07-07 dir-right)
    (MOVE-DIR pos-06-08 pos-05-08 dir-left)
    (MOVE-DIR pos-06-08 pos-06-07 dir-up)
    (MOVE-DIR pos-06-08 pos-07-08 dir-right)
    (MOVE-DIR pos-06-10 pos-07-10 dir-right)
    (MOVE-DIR pos-07-01 pos-08-01 dir-right)
    (MOVE-DIR pos-07-04 pos-06-04 dir-left)
    (MOVE-DIR pos-07-04 pos-07-05 dir-down)
    (MOVE-DIR pos-07-05 pos-06-05 dir-left)
    (MOVE-DIR pos-07-05 pos-07-04 dir-up)
    (MOVE-DIR pos-07-07 pos-06-07 dir-left)
    (MOVE-DIR pos-07-07 pos-07-08 dir-down)
    (MOVE-DIR pos-07-08 pos-06-08 dir-left)
    (MOVE-DIR pos-07-08 pos-07-07 dir-up)
    (MOVE-DIR pos-07-10 pos-06-10 dir-left)
    (MOVE-DIR pos-07-10 pos-08-10 dir-right)
    (MOVE-DIR pos-08-01 pos-07-01 dir-left)
    (MOVE-DIR pos-08-01 pos-08-02 dir-down)
    (MOVE-DIR pos-08-02 pos-08-01 dir-up)
    (MOVE-DIR pos-08-10 pos-07-10 dir-left)
    (at player-01 pos-04-08)
    (at stone-01 pos-03-03)
    (at stone-02 pos-05-03)
    (at stone-03 pos-04-04)
    (at stone-04 pos-05-04)
    (clear pos-01-05)
    (clear pos-01-06)
    (clear pos-01-07)
    (clear pos-01-08)
    (clear pos-01-09)
    (clear pos-01-10)
    (clear pos-02-02)
    (clear pos-02-03)
    (clear pos-03-02)
    (clear pos-03-04)
    (clear pos-03-05)
    (clear pos-03-06)
    (clear pos-03-07)
    (clear pos-03-08)
    (clear pos-03-09)
    (clear pos-04-02)
    (clear pos-04-03)
    (clear pos-04-07)
    (clear pos-04-09)
    (clear pos-05-02)
    (clear pos-05-05)
    (clear pos-05-07)
    (clear pos-05-08)
    (clear pos-06-03)
    (clear pos-06-04)
    (clear pos-06-05)
    (clear pos-06-06)
    (clear pos-06-07)
    (clear pos-06-08)
    (clear pos-06-10)
    (clear pos-07-01)
    (clear pos-07-04)
    (clear pos-07-05)
    (clear pos-07-07)
    (clear pos-07-08)
    (clear pos-07-10)
    (clear pos-08-01)
    (clear pos-08-02)
    (clear pos-08-10)
    (= (total-cost) 0)
  )
  (:goal (and
    (at-goal stone-01)
    (at-goal stone-02)
    (at-goal stone-03)
    (at-goal stone-04)
  ))
  (:metric minimize (total-cost))
)
