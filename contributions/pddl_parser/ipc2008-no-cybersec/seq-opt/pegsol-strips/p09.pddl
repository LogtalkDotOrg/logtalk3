;; The following problem is taken from the problem library of Solipeg 2.2:
;; 
;; Solipeg, a Classic Marble Puzzle Game for the
;; Psion Series 3a, 3c and Siena
;; Version 2.2 (and 2.2 Lite)
;; Copyright (C) 1993, 1994, 1995, 1996 J Cade Roux
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;; (see http://ourworld.compuserve.com/homepages/cade/psionsof.htm,
;; http://ourworld.compuserve.com/homepages/cade/solsrc22.zip)
;; 
;; The Solipeg problem library in turn is derived from the booklet
;; "Problems in Puzzle-Peg" included in the game Puzzle-Peg (Third
;; Edition, 1924, Lubbers & Bell Mfg. Co.,Clinton, Iowa, USA).
;; 
;; Original problem number and name: "68 From 21"
;; 
;; Number of pegs:                  12
;; Number of diagonal moves needed: 0
;; 
;; Problem description:
;; "*" denotes "occupied"
;; "o" denotes "free"
;; 
;;     * o o     
;;     o * o     
;; o o * * * o o 
;; o * o o * * o 
;; o o * * * o o 
;;     o * o     
;;     o o o     
;; 
;; Target position = (3,3)
;; 
(define (problem pegsolitaire-sequential-068)
    (:domain pegsolitaire-sequential)
    (:objects
        pos-0-2 - location
        pos-0-3 - location
        pos-0-4 - location
        pos-1-2 - location
        pos-1-3 - location
        pos-1-4 - location
        pos-2-0 - location
        pos-2-1 - location
        pos-2-2 - location
        pos-2-3 - location
        pos-2-4 - location
        pos-2-5 - location
        pos-2-6 - location
        pos-3-0 - location
        pos-3-1 - location
        pos-3-2 - location
        pos-3-3 - location
        pos-3-4 - location
        pos-3-5 - location
        pos-3-6 - location
        pos-4-0 - location
        pos-4-1 - location
        pos-4-2 - location
        pos-4-3 - location
        pos-4-4 - location
        pos-4-5 - location
        pos-4-6 - location
        pos-5-2 - location
        pos-5-3 - location
        pos-5-4 - location
        pos-6-2 - location
        pos-6-3 - location
        pos-6-4 - location
    )
    (:init
        (= (total-cost) 0)
        (move-ended)
        (IN-LINE pos-0-2 pos-0-3 pos-0-4)
        (IN-LINE pos-0-4 pos-0-3 pos-0-2)
        (IN-LINE pos-0-2 pos-1-2 pos-2-2)
        (IN-LINE pos-2-2 pos-1-2 pos-0-2)
        (IN-LINE pos-0-3 pos-1-3 pos-2-3)
        (IN-LINE pos-2-3 pos-1-3 pos-0-3)
        (IN-LINE pos-0-4 pos-1-4 pos-2-4)
        (IN-LINE pos-2-4 pos-1-4 pos-0-4)
        (IN-LINE pos-1-2 pos-1-3 pos-1-4)
        (IN-LINE pos-1-4 pos-1-3 pos-1-2)
        (IN-LINE pos-1-2 pos-2-2 pos-3-2)
        (IN-LINE pos-3-2 pos-2-2 pos-1-2)
        (IN-LINE pos-1-3 pos-2-3 pos-3-3)
        (IN-LINE pos-3-3 pos-2-3 pos-1-3)
        (IN-LINE pos-1-4 pos-2-4 pos-3-4)
        (IN-LINE pos-3-4 pos-2-4 pos-1-4)
        (IN-LINE pos-2-0 pos-2-1 pos-2-2)
        (IN-LINE pos-2-2 pos-2-1 pos-2-0)
        (IN-LINE pos-2-0 pos-3-0 pos-4-0)
        (IN-LINE pos-4-0 pos-3-0 pos-2-0)
        (IN-LINE pos-2-1 pos-2-2 pos-2-3)
        (IN-LINE pos-2-3 pos-2-2 pos-2-1)
        (IN-LINE pos-2-1 pos-3-1 pos-4-1)
        (IN-LINE pos-4-1 pos-3-1 pos-2-1)
        (IN-LINE pos-2-2 pos-2-3 pos-2-4)
        (IN-LINE pos-2-4 pos-2-3 pos-2-2)
        (IN-LINE pos-2-2 pos-3-2 pos-4-2)
        (IN-LINE pos-4-2 pos-3-2 pos-2-2)
        (IN-LINE pos-2-3 pos-2-4 pos-2-5)
        (IN-LINE pos-2-5 pos-2-4 pos-2-3)
        (IN-LINE pos-2-3 pos-3-3 pos-4-3)
        (IN-LINE pos-4-3 pos-3-3 pos-2-3)
        (IN-LINE pos-2-4 pos-2-5 pos-2-6)
        (IN-LINE pos-2-6 pos-2-5 pos-2-4)
        (IN-LINE pos-2-4 pos-3-4 pos-4-4)
        (IN-LINE pos-4-4 pos-3-4 pos-2-4)
        (IN-LINE pos-2-5 pos-3-5 pos-4-5)
        (IN-LINE pos-4-5 pos-3-5 pos-2-5)
        (IN-LINE pos-2-6 pos-3-6 pos-4-6)
        (IN-LINE pos-4-6 pos-3-6 pos-2-6)
        (IN-LINE pos-3-0 pos-3-1 pos-3-2)
        (IN-LINE pos-3-2 pos-3-1 pos-3-0)
        (IN-LINE pos-3-1 pos-3-2 pos-3-3)
        (IN-LINE pos-3-3 pos-3-2 pos-3-1)
        (IN-LINE pos-3-2 pos-3-3 pos-3-4)
        (IN-LINE pos-3-4 pos-3-3 pos-3-2)
        (IN-LINE pos-3-2 pos-4-2 pos-5-2)
        (IN-LINE pos-5-2 pos-4-2 pos-3-2)
        (IN-LINE pos-3-3 pos-3-4 pos-3-5)
        (IN-LINE pos-3-5 pos-3-4 pos-3-3)
        (IN-LINE pos-3-3 pos-4-3 pos-5-3)
        (IN-LINE pos-5-3 pos-4-3 pos-3-3)
        (IN-LINE pos-3-4 pos-3-5 pos-3-6)
        (IN-LINE pos-3-6 pos-3-5 pos-3-4)
        (IN-LINE pos-3-4 pos-4-4 pos-5-4)
        (IN-LINE pos-5-4 pos-4-4 pos-3-4)
        (IN-LINE pos-4-0 pos-4-1 pos-4-2)
        (IN-LINE pos-4-2 pos-4-1 pos-4-0)
        (IN-LINE pos-4-1 pos-4-2 pos-4-3)
        (IN-LINE pos-4-3 pos-4-2 pos-4-1)
        (IN-LINE pos-4-2 pos-4-3 pos-4-4)
        (IN-LINE pos-4-4 pos-4-3 pos-4-2)
        (IN-LINE pos-4-2 pos-5-2 pos-6-2)
        (IN-LINE pos-6-2 pos-5-2 pos-4-2)
        (IN-LINE pos-4-3 pos-4-4 pos-4-5)
        (IN-LINE pos-4-5 pos-4-4 pos-4-3)
        (IN-LINE pos-4-3 pos-5-3 pos-6-3)
        (IN-LINE pos-6-3 pos-5-3 pos-4-3)
        (IN-LINE pos-4-4 pos-4-5 pos-4-6)
        (IN-LINE pos-4-6 pos-4-5 pos-4-4)
        (IN-LINE pos-4-4 pos-5-4 pos-6-4)
        (IN-LINE pos-6-4 pos-5-4 pos-4-4)
        (IN-LINE pos-5-2 pos-5-3 pos-5-4)
        (IN-LINE pos-5-4 pos-5-3 pos-5-2)
        (IN-LINE pos-6-2 pos-6-3 pos-6-4)
        (IN-LINE pos-6-4 pos-6-3 pos-6-2)
        (free pos-0-3)
        (free pos-0-4)
        (free pos-1-2)
        (free pos-1-4)
        (free pos-2-0)
        (free pos-2-1)
        (free pos-2-5)
        (free pos-2-6)
        (free pos-3-0)
        (free pos-3-2)
        (free pos-3-3)
        (free pos-3-6)
        (free pos-4-0)
        (free pos-4-1)
        (free pos-4-5)
        (free pos-4-6)
        (free pos-5-2)
        (free pos-5-4)
        (free pos-6-2)
        (free pos-6-3)
        (free pos-6-4)
        (occupied pos-0-2)
        (occupied pos-1-3)
        (occupied pos-2-2)
        (occupied pos-2-3)
        (occupied pos-2-4)
        (occupied pos-3-1)
        (occupied pos-3-4)
        (occupied pos-3-5)
        (occupied pos-4-2)
        (occupied pos-4-3)
        (occupied pos-4-4)
        (occupied pos-5-3)
    )
    (:goal (and
        (free pos-0-2)
        (free pos-0-3)
        (free pos-0-4)
        (free pos-1-2)
        (free pos-1-3)
        (free pos-1-4)
        (free pos-2-0)
        (free pos-2-1)
        (free pos-2-2)
        (free pos-2-3)
        (free pos-2-4)
        (free pos-2-5)
        (free pos-2-6)
        (free pos-3-0)
        (free pos-3-1)
        (free pos-3-2)
        (free pos-3-4)
        (free pos-3-5)
        (free pos-3-6)
        (free pos-4-0)
        (free pos-4-1)
        (free pos-4-2)
        (free pos-4-3)
        (free pos-4-4)
        (free pos-4-5)
        (free pos-4-6)
        (free pos-5-2)
        (free pos-5-3)
        (free pos-5-4)
        (free pos-6-2)
        (free pos-6-3)
        (free pos-6-4)
        (occupied pos-3-3)
           )
    )
    (:metric minimize (total-cost))
)
