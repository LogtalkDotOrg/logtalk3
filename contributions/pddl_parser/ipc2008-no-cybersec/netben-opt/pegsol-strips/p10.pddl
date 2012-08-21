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
;; Original problem number and name: "89 Shift Play"
;; 
;; Number of pegs:                  11
;; Number of diagonal moves needed: 5
;; 
;; Problem description:
;; "*" denotes "occupied"
;; "o" denotes "free"
;; 
;;     o o o     
;;     o * o     
;; o * * * o * o 
;; * * o * o o o 
;; o * o o o * o 
;;     o * o     
;;     o o o     
;; 
;; Target position = (3,3)
;; 
(define (problem pegsolitaire-netbenefit-089-3)
    (:domain pegsolitaire-netbenefit)
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
        (free pos-0-2)
        (free pos-0-3)
        (free pos-0-4)
        (free pos-1-2)
        (free pos-1-4)
        (free pos-2-0)
        (free pos-2-4)
        (free pos-2-6)
        (free pos-3-2)
        (free pos-3-4)
        (free pos-3-5)
        (free pos-3-6)
        (free pos-4-0)
        (free pos-4-2)
        (free pos-4-3)
        (free pos-4-4)
        (free pos-4-6)
        (free pos-5-2)
        (free pos-5-4)
        (free pos-6-2)
        (free pos-6-3)
        (free pos-6-4)
        (occupied pos-1-3)
        (occupied pos-2-1)
        (occupied pos-2-2)
        (occupied pos-2-3)
        (occupied pos-2-5)
        (occupied pos-3-0)
        (occupied pos-3-1)
        (occupied pos-3-3)
        (occupied pos-4-1)
        (occupied pos-4-5)
        (occupied pos-5-3)
    )
    (:goal (and
        (preference g1 (free pos-0-2))
        (preference g2 (free pos-0-3))
        (preference g3 (free pos-0-4))
        (preference g4 (free pos-1-2))
        (preference g5 (free pos-1-3))
        (preference g6 (free pos-1-4))
        (preference g7 (free pos-2-0))
        (preference g8 (free pos-2-1))
        (preference g9 (free pos-2-2))
        (preference g10 (free pos-2-3))
        (preference g11 (free pos-2-4))
        (preference g12 (free pos-2-5))
        (preference g13 (free pos-2-6))
        (preference g14 (free pos-3-0))
        (preference g15 (free pos-3-1))
        (preference g16 (free pos-3-2))
        (preference g17 (free pos-3-3))
        (preference g18 (free pos-3-4))
        (preference g19 (free pos-3-5))
        (preference g20 (free pos-3-6))
        (preference g21 (free pos-4-0))
        (preference g22 (free pos-4-1))
        (preference g23 (free pos-4-2))
        (preference g24 (free pos-4-3))
        (preference g25 (free pos-4-4))
        (preference g26 (free pos-4-5))
        (preference g27 (free pos-4-6))
        (preference g28 (free pos-5-2))
        (preference g29 (free pos-5-3))
        (preference g30 (free pos-5-4))
        (preference g31 (free pos-6-2))
        (preference g32 (free pos-6-3))
        (preference g33 (free pos-6-4))
           )
    )
    (:metric maximize (- 64 (+
        (* (is-violated g1) 1)
        (* (is-violated g2) 3)
        (* (is-violated g3) 1)
        (* (is-violated g4) 3)
        (* (is-violated g5) 6)
        (* (is-violated g6) 3)
        (* (is-violated g7) 1)
        (* (is-violated g8) 3)
        (* (is-violated g9) 6)
        (* (is-violated g10) 10)
        (* (is-violated g11) 6)
        (* (is-violated g12) 3)
        (* (is-violated g13) 1)
        (* (is-violated g14) 3)
        (* (is-violated g15) 6)
        (* (is-violated g16) 10)
        (* (is-violated g17) 15)
        (* (is-violated g18) 10)
        (* (is-violated g19) 6)
        (* (is-violated g20) 3)
        (* (is-violated g21) 1)
        (* (is-violated g22) 3)
        (* (is-violated g23) 6)
        (* (is-violated g24) 10)
        (* (is-violated g25) 6)
        (* (is-violated g26) 3)
        (* (is-violated g27) 1)
        (* (is-violated g28) 3)
        (* (is-violated g29) 6)
        (* (is-violated g30) 3)
        (* (is-violated g31) 1)
        (* (is-violated g32) 3)
        (* (is-violated g33) 1)
    )))
)
