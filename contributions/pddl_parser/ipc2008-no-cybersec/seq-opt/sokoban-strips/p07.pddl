;;    ###
;;    #@#
;;  ###$###
;; ##  .  ##
;; #  # #  #
;; # #   # #
;; # #   # #
;; # #   # #
;; #  # #  #
;; ## $ $ ##
;;  ##. .##
;;   #   #
;;   #   #
;;   #####

(define (problem p066-microban-sequential)
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
    pos-01-11 - location
    pos-01-12 - location
    pos-01-13 - location
    pos-01-14 - location
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
    pos-02-11 - location
    pos-02-12 - location
    pos-02-13 - location
    pos-02-14 - location
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
    pos-03-11 - location
    pos-03-12 - location
    pos-03-13 - location
    pos-03-14 - location
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
    pos-04-11 - location
    pos-04-12 - location
    pos-04-13 - location
    pos-04-14 - location
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
    pos-05-11 - location
    pos-05-12 - location
    pos-05-13 - location
    pos-05-14 - location
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
    pos-06-11 - location
    pos-06-12 - location
    pos-06-13 - location
    pos-06-14 - location
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
    pos-07-11 - location
    pos-07-12 - location
    pos-07-13 - location
    pos-07-14 - location
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
    pos-08-11 - location
    pos-08-12 - location
    pos-08-13 - location
    pos-08-14 - location
    pos-09-01 - location
    pos-09-02 - location
    pos-09-03 - location
    pos-09-04 - location
    pos-09-05 - location
    pos-09-06 - location
    pos-09-07 - location
    pos-09-08 - location
    pos-09-09 - location
    pos-09-10 - location
    pos-09-11 - location
    pos-09-12 - location
    pos-09-13 - location
    pos-09-14 - location
    stone-01 - stone
    stone-02 - stone
    stone-03 - stone
  )
  (:init
    (IS-GOAL pos-04-11)
    (IS-GOAL pos-05-04)
    (IS-GOAL pos-06-11)
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
    (IS-NONGOAL pos-01-11)
    (IS-NONGOAL pos-01-12)
    (IS-NONGOAL pos-01-13)
    (IS-NONGOAL pos-01-14)
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
    (IS-NONGOAL pos-02-11)
    (IS-NONGOAL pos-02-12)
    (IS-NONGOAL pos-02-13)
    (IS-NONGOAL pos-02-14)
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
    (IS-NONGOAL pos-03-11)
    (IS-NONGOAL pos-03-12)
    (IS-NONGOAL pos-03-13)
    (IS-NONGOAL pos-03-14)
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
    (IS-NONGOAL pos-04-12)
    (IS-NONGOAL pos-04-13)
    (IS-NONGOAL pos-04-14)
    (IS-NONGOAL pos-05-01)
    (IS-NONGOAL pos-05-02)
    (IS-NONGOAL pos-05-03)
    (IS-NONGOAL pos-05-05)
    (IS-NONGOAL pos-05-06)
    (IS-NONGOAL pos-05-07)
    (IS-NONGOAL pos-05-08)
    (IS-NONGOAL pos-05-09)
    (IS-NONGOAL pos-05-10)
    (IS-NONGOAL pos-05-11)
    (IS-NONGOAL pos-05-12)
    (IS-NONGOAL pos-05-13)
    (IS-NONGOAL pos-05-14)
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
    (IS-NONGOAL pos-06-12)
    (IS-NONGOAL pos-06-13)
    (IS-NONGOAL pos-06-14)
    (IS-NONGOAL pos-07-01)
    (IS-NONGOAL pos-07-02)
    (IS-NONGOAL pos-07-03)
    (IS-NONGOAL pos-07-04)
    (IS-NONGOAL pos-07-05)
    (IS-NONGOAL pos-07-06)
    (IS-NONGOAL pos-07-07)
    (IS-NONGOAL pos-07-08)
    (IS-NONGOAL pos-07-09)
    (IS-NONGOAL pos-07-10)
    (IS-NONGOAL pos-07-11)
    (IS-NONGOAL pos-07-12)
    (IS-NONGOAL pos-07-13)
    (IS-NONGOAL pos-07-14)
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
    (IS-NONGOAL pos-08-11)
    (IS-NONGOAL pos-08-12)
    (IS-NONGOAL pos-08-13)
    (IS-NONGOAL pos-08-14)
    (IS-NONGOAL pos-09-01)
    (IS-NONGOAL pos-09-02)
    (IS-NONGOAL pos-09-03)
    (IS-NONGOAL pos-09-04)
    (IS-NONGOAL pos-09-05)
    (IS-NONGOAL pos-09-06)
    (IS-NONGOAL pos-09-07)
    (IS-NONGOAL pos-09-08)
    (IS-NONGOAL pos-09-09)
    (IS-NONGOAL pos-09-10)
    (IS-NONGOAL pos-09-11)
    (IS-NONGOAL pos-09-12)
    (IS-NONGOAL pos-09-13)
    (IS-NONGOAL pos-09-14)
    (MOVE-DIR pos-01-01 pos-01-02 dir-down)
    (MOVE-DIR pos-01-01 pos-02-01 dir-right)
    (MOVE-DIR pos-01-02 pos-01-01 dir-up)
    (MOVE-DIR pos-01-02 pos-01-03 dir-down)
    (MOVE-DIR pos-01-02 pos-02-02 dir-right)
    (MOVE-DIR pos-01-03 pos-01-02 dir-up)
    (MOVE-DIR pos-01-11 pos-01-12 dir-down)
    (MOVE-DIR pos-01-12 pos-01-11 dir-up)
    (MOVE-DIR pos-01-12 pos-01-13 dir-down)
    (MOVE-DIR pos-01-12 pos-02-12 dir-right)
    (MOVE-DIR pos-01-13 pos-01-12 dir-up)
    (MOVE-DIR pos-01-13 pos-01-14 dir-down)
    (MOVE-DIR pos-01-13 pos-02-13 dir-right)
    (MOVE-DIR pos-01-14 pos-01-13 dir-up)
    (MOVE-DIR pos-01-14 pos-02-14 dir-right)
    (MOVE-DIR pos-02-01 pos-01-01 dir-left)
    (MOVE-DIR pos-02-01 pos-02-02 dir-down)
    (MOVE-DIR pos-02-01 pos-03-01 dir-right)
    (MOVE-DIR pos-02-02 pos-01-02 dir-left)
    (MOVE-DIR pos-02-02 pos-02-01 dir-up)
    (MOVE-DIR pos-02-02 pos-03-02 dir-right)
    (MOVE-DIR pos-02-05 pos-02-06 dir-down)
    (MOVE-DIR pos-02-05 pos-03-05 dir-right)
    (MOVE-DIR pos-02-06 pos-02-05 dir-up)
    (MOVE-DIR pos-02-06 pos-02-07 dir-down)
    (MOVE-DIR pos-02-07 pos-02-06 dir-up)
    (MOVE-DIR pos-02-07 pos-02-08 dir-down)
    (MOVE-DIR pos-02-08 pos-02-07 dir-up)
    (MOVE-DIR pos-02-08 pos-02-09 dir-down)
    (MOVE-DIR pos-02-09 pos-02-08 dir-up)
    (MOVE-DIR pos-02-09 pos-03-09 dir-right)
    (MOVE-DIR pos-02-12 pos-01-12 dir-left)
    (MOVE-DIR pos-02-12 pos-02-13 dir-down)
    (MOVE-DIR pos-02-13 pos-01-13 dir-left)
    (MOVE-DIR pos-02-13 pos-02-12 dir-up)
    (MOVE-DIR pos-02-13 pos-02-14 dir-down)
    (MOVE-DIR pos-02-14 pos-01-14 dir-left)
    (MOVE-DIR pos-02-14 pos-02-13 dir-up)
    (MOVE-DIR pos-03-01 pos-02-01 dir-left)
    (MOVE-DIR pos-03-01 pos-03-02 dir-down)
    (MOVE-DIR pos-03-02 pos-02-02 dir-left)
    (MOVE-DIR pos-03-02 pos-03-01 dir-up)
    (MOVE-DIR pos-03-04 pos-03-05 dir-down)
    (MOVE-DIR pos-03-04 pos-04-04 dir-right)
    (MOVE-DIR pos-03-05 pos-02-05 dir-left)
    (MOVE-DIR pos-03-05 pos-03-04 dir-up)
    (MOVE-DIR pos-03-09 pos-02-09 dir-left)
    (MOVE-DIR pos-03-09 pos-03-10 dir-down)
    (MOVE-DIR pos-03-10 pos-03-09 dir-up)
    (MOVE-DIR pos-03-10 pos-04-10 dir-right)
    (MOVE-DIR pos-04-04 pos-03-04 dir-left)
    (MOVE-DIR pos-04-04 pos-05-04 dir-right)
    (MOVE-DIR pos-04-06 pos-04-07 dir-down)
    (MOVE-DIR pos-04-06 pos-05-06 dir-right)
    (MOVE-DIR pos-04-07 pos-04-06 dir-up)
    (MOVE-DIR pos-04-07 pos-04-08 dir-down)
    (MOVE-DIR pos-04-07 pos-05-07 dir-right)
    (MOVE-DIR pos-04-08 pos-04-07 dir-up)
    (MOVE-DIR pos-04-08 pos-05-08 dir-right)
    (MOVE-DIR pos-04-10 pos-03-10 dir-left)
    (MOVE-DIR pos-04-10 pos-04-11 dir-down)
    (MOVE-DIR pos-04-10 pos-05-10 dir-right)
    (MOVE-DIR pos-04-11 pos-04-10 dir-up)
    (MOVE-DIR pos-04-11 pos-04-12 dir-down)
    (MOVE-DIR pos-04-11 pos-05-11 dir-right)
    (MOVE-DIR pos-04-12 pos-04-11 dir-up)
    (MOVE-DIR pos-04-12 pos-04-13 dir-down)
    (MOVE-DIR pos-04-12 pos-05-12 dir-right)
    (MOVE-DIR pos-04-13 pos-04-12 dir-up)
    (MOVE-DIR pos-04-13 pos-05-13 dir-right)
    (MOVE-DIR pos-05-02 pos-05-03 dir-down)
    (MOVE-DIR pos-05-03 pos-05-02 dir-up)
    (MOVE-DIR pos-05-03 pos-05-04 dir-down)
    (MOVE-DIR pos-05-04 pos-04-04 dir-left)
    (MOVE-DIR pos-05-04 pos-05-03 dir-up)
    (MOVE-DIR pos-05-04 pos-05-05 dir-down)
    (MOVE-DIR pos-05-04 pos-06-04 dir-right)
    (MOVE-DIR pos-05-05 pos-05-04 dir-up)
    (MOVE-DIR pos-05-05 pos-05-06 dir-down)
    (MOVE-DIR pos-05-06 pos-04-06 dir-left)
    (MOVE-DIR pos-05-06 pos-05-05 dir-up)
    (MOVE-DIR pos-05-06 pos-05-07 dir-down)
    (MOVE-DIR pos-05-06 pos-06-06 dir-right)
    (MOVE-DIR pos-05-07 pos-04-07 dir-left)
    (MOVE-DIR pos-05-07 pos-05-06 dir-up)
    (MOVE-DIR pos-05-07 pos-05-08 dir-down)
    (MOVE-DIR pos-05-07 pos-06-07 dir-right)
    (MOVE-DIR pos-05-08 pos-04-08 dir-left)
    (MOVE-DIR pos-05-08 pos-05-07 dir-up)
    (MOVE-DIR pos-05-08 pos-05-09 dir-down)
    (MOVE-DIR pos-05-08 pos-06-08 dir-right)
    (MOVE-DIR pos-05-09 pos-05-08 dir-up)
    (MOVE-DIR pos-05-09 pos-05-10 dir-down)
    (MOVE-DIR pos-05-10 pos-04-10 dir-left)
    (MOVE-DIR pos-05-10 pos-05-09 dir-up)
    (MOVE-DIR pos-05-10 pos-05-11 dir-down)
    (MOVE-DIR pos-05-10 pos-06-10 dir-right)
    (MOVE-DIR pos-05-11 pos-04-11 dir-left)
    (MOVE-DIR pos-05-11 pos-05-10 dir-up)
    (MOVE-DIR pos-05-11 pos-05-12 dir-down)
    (MOVE-DIR pos-05-11 pos-06-11 dir-right)
    (MOVE-DIR pos-05-12 pos-04-12 dir-left)
    (MOVE-DIR pos-05-12 pos-05-11 dir-up)
    (MOVE-DIR pos-05-12 pos-05-13 dir-down)
    (MOVE-DIR pos-05-12 pos-06-12 dir-right)
    (MOVE-DIR pos-05-13 pos-04-13 dir-left)
    (MOVE-DIR pos-05-13 pos-05-12 dir-up)
    (MOVE-DIR pos-05-13 pos-06-13 dir-right)
    (MOVE-DIR pos-06-04 pos-05-04 dir-left)
    (MOVE-DIR pos-06-04 pos-07-04 dir-right)
    (MOVE-DIR pos-06-06 pos-05-06 dir-left)
    (MOVE-DIR pos-06-06 pos-06-07 dir-down)
    (MOVE-DIR pos-06-07 pos-05-07 dir-left)
    (MOVE-DIR pos-06-07 pos-06-06 dir-up)
    (MOVE-DIR pos-06-07 pos-06-08 dir-down)
    (MOVE-DIR pos-06-08 pos-05-08 dir-left)
    (MOVE-DIR pos-06-08 pos-06-07 dir-up)
    (MOVE-DIR pos-06-10 pos-05-10 dir-left)
    (MOVE-DIR pos-06-10 pos-06-11 dir-down)
    (MOVE-DIR pos-06-10 pos-07-10 dir-right)
    (MOVE-DIR pos-06-11 pos-05-11 dir-left)
    (MOVE-DIR pos-06-11 pos-06-10 dir-up)
    (MOVE-DIR pos-06-11 pos-06-12 dir-down)
    (MOVE-DIR pos-06-12 pos-05-12 dir-left)
    (MOVE-DIR pos-06-12 pos-06-11 dir-up)
    (MOVE-DIR pos-06-12 pos-06-13 dir-down)
    (MOVE-DIR pos-06-13 pos-05-13 dir-left)
    (MOVE-DIR pos-06-13 pos-06-12 dir-up)
    (MOVE-DIR pos-07-01 pos-07-02 dir-down)
    (MOVE-DIR pos-07-01 pos-08-01 dir-right)
    (MOVE-DIR pos-07-02 pos-07-01 dir-up)
    (MOVE-DIR pos-07-02 pos-08-02 dir-right)
    (MOVE-DIR pos-07-04 pos-06-04 dir-left)
    (MOVE-DIR pos-07-04 pos-07-05 dir-down)
    (MOVE-DIR pos-07-05 pos-07-04 dir-up)
    (MOVE-DIR pos-07-05 pos-08-05 dir-right)
    (MOVE-DIR pos-07-09 pos-07-10 dir-down)
    (MOVE-DIR pos-07-09 pos-08-09 dir-right)
    (MOVE-DIR pos-07-10 pos-06-10 dir-left)
    (MOVE-DIR pos-07-10 pos-07-09 dir-up)
    (MOVE-DIR pos-08-01 pos-07-01 dir-left)
    (MOVE-DIR pos-08-01 pos-08-02 dir-down)
    (MOVE-DIR pos-08-01 pos-09-01 dir-right)
    (MOVE-DIR pos-08-02 pos-07-02 dir-left)
    (MOVE-DIR pos-08-02 pos-08-01 dir-up)
    (MOVE-DIR pos-08-02 pos-09-02 dir-right)
    (MOVE-DIR pos-08-05 pos-07-05 dir-left)
    (MOVE-DIR pos-08-05 pos-08-06 dir-down)
    (MOVE-DIR pos-08-06 pos-08-05 dir-up)
    (MOVE-DIR pos-08-06 pos-08-07 dir-down)
    (MOVE-DIR pos-08-07 pos-08-06 dir-up)
    (MOVE-DIR pos-08-07 pos-08-08 dir-down)
    (MOVE-DIR pos-08-08 pos-08-07 dir-up)
    (MOVE-DIR pos-08-08 pos-08-09 dir-down)
    (MOVE-DIR pos-08-09 pos-07-09 dir-left)
    (MOVE-DIR pos-08-09 pos-08-08 dir-up)
    (MOVE-DIR pos-08-12 pos-08-13 dir-down)
    (MOVE-DIR pos-08-12 pos-09-12 dir-right)
    (MOVE-DIR pos-08-13 pos-08-12 dir-up)
    (MOVE-DIR pos-08-13 pos-08-14 dir-down)
    (MOVE-DIR pos-08-13 pos-09-13 dir-right)
    (MOVE-DIR pos-08-14 pos-08-13 dir-up)
    (MOVE-DIR pos-08-14 pos-09-14 dir-right)
    (MOVE-DIR pos-09-01 pos-08-01 dir-left)
    (MOVE-DIR pos-09-01 pos-09-02 dir-down)
    (MOVE-DIR pos-09-02 pos-08-02 dir-left)
    (MOVE-DIR pos-09-02 pos-09-01 dir-up)
    (MOVE-DIR pos-09-02 pos-09-03 dir-down)
    (MOVE-DIR pos-09-03 pos-09-02 dir-up)
    (MOVE-DIR pos-09-11 pos-09-12 dir-down)
    (MOVE-DIR pos-09-12 pos-08-12 dir-left)
    (MOVE-DIR pos-09-12 pos-09-11 dir-up)
    (MOVE-DIR pos-09-12 pos-09-13 dir-down)
    (MOVE-DIR pos-09-13 pos-08-13 dir-left)
    (MOVE-DIR pos-09-13 pos-09-12 dir-up)
    (MOVE-DIR pos-09-13 pos-09-14 dir-down)
    (MOVE-DIR pos-09-14 pos-08-14 dir-left)
    (MOVE-DIR pos-09-14 pos-09-13 dir-up)
    (at player-01 pos-05-02)
    (at stone-01 pos-05-03)
    (at stone-02 pos-04-10)
    (at stone-03 pos-06-10)
    (clear pos-01-01)
    (clear pos-01-02)
    (clear pos-01-03)
    (clear pos-01-11)
    (clear pos-01-12)
    (clear pos-01-13)
    (clear pos-01-14)
    (clear pos-02-01)
    (clear pos-02-02)
    (clear pos-02-05)
    (clear pos-02-06)
    (clear pos-02-07)
    (clear pos-02-08)
    (clear pos-02-09)
    (clear pos-02-12)
    (clear pos-02-13)
    (clear pos-02-14)
    (clear pos-03-01)
    (clear pos-03-02)
    (clear pos-03-04)
    (clear pos-03-05)
    (clear pos-03-09)
    (clear pos-03-10)
    (clear pos-04-04)
    (clear pos-04-06)
    (clear pos-04-07)
    (clear pos-04-08)
    (clear pos-04-11)
    (clear pos-04-12)
    (clear pos-04-13)
    (clear pos-05-04)
    (clear pos-05-05)
    (clear pos-05-06)
    (clear pos-05-07)
    (clear pos-05-08)
    (clear pos-05-09)
    (clear pos-05-10)
    (clear pos-05-11)
    (clear pos-05-12)
    (clear pos-05-13)
    (clear pos-06-04)
    (clear pos-06-06)
    (clear pos-06-07)
    (clear pos-06-08)
    (clear pos-06-11)
    (clear pos-06-12)
    (clear pos-06-13)
    (clear pos-07-01)
    (clear pos-07-02)
    (clear pos-07-04)
    (clear pos-07-05)
    (clear pos-07-09)
    (clear pos-07-10)
    (clear pos-08-01)
    (clear pos-08-02)
    (clear pos-08-05)
    (clear pos-08-06)
    (clear pos-08-07)
    (clear pos-08-08)
    (clear pos-08-09)
    (clear pos-08-12)
    (clear pos-08-13)
    (clear pos-08-14)
    (clear pos-09-01)
    (clear pos-09-02)
    (clear pos-09-03)
    (clear pos-09-11)
    (clear pos-09-12)
    (clear pos-09-13)
    (clear pos-09-14)
    (= (total-cost) 0)
  )
  (:goal (and
    (at-goal stone-01)
    (at-goal stone-02)
    (at-goal stone-03)
  ))
  (:metric minimize (total-cost))
)
