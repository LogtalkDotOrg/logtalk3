---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# searching

Some code in this folder is adapted, with permission, from the book 
"Prolog Programming for Artificial Intelligence" by Ivan Bratko.

For a description of the search problems, please see a classical AI book 
(such as the one above) or visit <http://www.plastelina.net/games>.

This example defines two hierarchies of objects, one for representing 
state-spaces and another for representing search methods:

```text
state_space
	farmer
	water_jug
	salt(Quantity, Measure1, Measure2)
	heuristic_state_space
		bridge
		eight_puzzle
		miss_cann

search_strategy
	blind_search(Bound)
		breadth_first(Bound)
		depth_first(Bound)
	heuristic_search(Threshold)
		best_first(Threshold)
		hill_climbing(Threshold)
```

Taken together, these two hierarchies implement a framework for solving 
state-space search problems in Logtalk. There is also a monitor object, 
`performance`, which tries to measure the time taken to find a solution, 
the branching factor while searching for a solution, and the number of 
transitions made to find a solution.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(searching(loader)).
```

The farmer, cabbage, goat and wolf problem:

```logtalk
farmer::initial_state(Initial), depth_first(10)::solve(farmer, Initial, Path), farmer::print_path(Path).
```

<!--
cgwf.<__>..........____
c_w_..........<__>.f_g_
c_wf.<__>..........__g_
__w_..........<__>.fcg_
_gwf.<__>.........._c__
_g__..........<__>.fc_w
_g_f.<__>.........._c_w
____..........<__>.fcgw

Path = [(north,north,north,north),(north,south,north,south),(north,south,north,north),(south,south,north,south),(south,north,north,north),(south,north,south,south),(south,north,south,north),(south,south,south,south)],
Initial = (north,north,north,north) ? 

true.
-->

Missionaries and cannibals problem, solved using a hill-climbing strategy:

```logtalk
miss_cann::initial_state(Initial), hill_climbing(16)::solve(miss_cann, Initial, Path, Cost), miss_cann::print_path(Path).
```

<!--
MMMCCC.<__>..........
MMCC..........<__>.MC
MMMCC.<__>..........C
MMM..........<__>.CCC
MMMC.<__>..........CC
MC..........<__>.MMCC
MMCC.<__>..........MC
CC..........<__>.MMMC
CCC.<__>..........MMM
C..........<__>.MMMCC
CC.<__>..........MMMC
..........<__>.MMMCCC

Cost = 15,
Path = [((3,3),left,0,0),((2,2),right,1,1),((3,2),left,0,1),((3,0),right,0,3),((3,1),left,0,2),((1,1),right,2,2),((2,2),left,1,1),((0,2),right,3,1),((0,3),left,3,0),((0,1),right,3,2),((0,2),left,3,1),((0,0),right,3,3)],
Initial = ((3,3),left,0,0)

true.
-->

Same problem as above with the addition of a monitor to measure hill-climbing performance:

```logtalk
performance::init, miss_cann::initial_state(Initial), hill_climbing(16)::solve(miss_cann, Initial, Path, Cost), miss_cann::print_path(Path), performance::report.
```

<!--
MMMCCC.<__>..........
MMCC..........<__>.MC
MMMCC.<__>..........C
MMM..........<__>.CCC
MMMC.<__>..........CC
MC..........<__>.MMCC
MMCC.<__>..........MC
CC..........<__>.MMMC
CCC.<__>..........MMM
C..........<__>.MMMCC
CC.<__>..........MMMC
..........<__>.MMMCCC
solution length: 12
number of state transitions: 26
ratio solution length / state transitions: 0.461538
minimum branching degree: 1
average branching degree: 2.30769
maximum branching degree: 3
time: 0.02

Cost = 15,
Path = [((3,3),left,0,0),((2,2),right,1,1),((3,2),left,0,1),((3,0),right,0,3),((3,1),left,0,2),((1,1),right,2,2),((2,2),left,1,1),((0,2),right,3,1),((0,3),left,3,0),((0,1),right,3,2),((0,2),left,3,1),((0,0),right,3,3)],
Initial = ((3,3),left,0,0) ? 

true.
-->

Bridge problem, solved using a hill climbing strategy:

```logtalk
performance::init, bridge::initial_state(Initial), hill_climbing(30)::solve(bridge, Initial, Path, Cost), bridge::print_path(Path), performance::report.
```

<!--
 _|____________|_ lamp 1 3 6 8 12 
1 3  lamp _|____________|_ 6 8 12 
3  _|____________|_ lamp 1 6 8 12 
1 3 6  lamp _|____________|_ 8 12 
3 6  _|____________|_ lamp 1 8 12 
3 6 8 12  lamp _|____________|_ 1 
6 8 12  _|____________|_ lamp 1 3 
1 3 6 8 12  lamp _|____________|_ 
solution length: 8
state transitions (including previous solutions): 555
ratio solution length / state transitions: 0.014414414414414415
minimum branching degree: 1
average branching degree: 7.32579185520362
maximum branching degree: 15
time: 0.012381000000000086
Initial = s([], right, [1, 3, 6, 8, 12]),
Path = [s([], right, [1, 3, 6, 8, 12]), s([1, 3], left, [6, 8, 12]), s([3], right, [1, 6, 8, 12]), s([1, 3, 6], left, [8, 12]), s([3, 6], right, [1, 8, 12]), s([3, 6|...], left, [1]), s([6|...], right, [1|...]), s([...|...], left, [])],
Cost = 29

true.
-->

Water jugs problem solved using a breadth and a depth first strategy, with performance monitors
it's interesting to compare the results:

```logtalk
performance::init, water_jug::initial_state(Initial), breadth_first(6)::solve(water_jug, Initial, Path), water_jug::print_path(Path), performance::report.
```

<!--
4-gallon jug: 0
3-gallon jug: 0

4-gallon jug: 0
3-gallon jug: 3

4-gallon jug: 3
3-gallon jug: 0

4-gallon jug: 3
3-gallon jug: 3

4-gallon jug: 4
3-gallon jug: 2

4-gallon jug: 0
3-gallon jug: 2

solution length: 6
number of state transitions: 109
ratio solution length / state transitions: 0.0550459
minimum branching degree: 2
average branching degree: 3.63158
maximum branching degree: 4
time: 0.02

Path = [(0,0),(0,3),(3,0),(3,3),(4,2),(0,2)],
Initial = (0,0) ? 

true.
-->

```logtalk
performance::init, water_jug::initial_state(Initial), depth_first(10)::solve(water_jug, Initial, Path), water_jug::print_path(Path), performance::report.
```

<!--
4-gallon jug: 0
3-gallon jug: 0

4-gallon jug: 4
3-gallon jug: 0

4-gallon jug: 4
3-gallon jug: 3

4-gallon jug: 0
3-gallon jug: 3

4-gallon jug: 3
3-gallon jug: 0

4-gallon jug: 3
3-gallon jug: 3

4-gallon jug: 4
3-gallon jug: 2

4-gallon jug: 0
3-gallon jug: 2

solution length: 8
number of state transitions: 12
ratio solution length / state transitions: 0.666667
minimum branching degree: 1
average branching degree: 2
maximum branching degree: 3
time: 0.00

Path = [(0,0),(4,0),(4,3),(0,3),(3,0),(3,3),(4,2),(0,2)],
Initial = (0,0) ? 

true.
-->

Salt puzzle using breadth first search

```logtalk
performance::init, salt(100, 500, 200)::initial_state(Initial), breadth_first(6)::solve(salt(100, 500, 200), Initial, Path), salt(100, 500, 200)::print_path(Path), performance::report.
```

<!--
(0, 0, 0)	all_empty
(0, 500, 0)	fill(m1)
(0, 300, 200)	transfer(m1, m2)
(0, 300, 0)	empty(m2)
(0, 100, 200)	transfer(m1, m2)
(100, 0, 200)	transfer(m1, acc)
solution length: 6
state transitions (including previous solutions): 405
ratio solution length / state transitions: 0.0148148
minimum branching degree: 1
average branching degree: 4.06863
maximum branching degree: 6
time: 0.03
Initial = (0, 0, 0, all_empty),
Path = [ (0, 0, 0, all_empty), (0, 500, 0, fill(m1)), (0, 300, 200, transfer(m1, m2)), (0, 300, 0, empty(m2)), (0, 100, 200, transfer(m1, m2)), (100, 0, 200, transfer(..., ...))] .

true.
-->

```logtalk
performance::init, salt(200, 250, 550)::initial_state(Initial), breadth_first(7)::solve(salt(200, 250, 550), Initial, Path), salt(200, 250, 550)::print_path(Path), performance::report.
```

<!--
(0, 0, 0)	all_empty
(0, 250, 0)	fill(m1)
(0, 0, 250)	transfer(m1, m2)
(0, 250, 250)	fill(m1)
(0, 0, 500)	transfer(m1, m2)
(0, 250, 500)	fill(m1)
(0, 200, 550)	transfer(m1, m2)
(200, 0, 550)	transfer(m1, acc)
solution length: 8
state transitions (including previous solutions): 2475
ratio solution length / state transitions: 0.00323232
minimum branching degree: 1
average branching degree: 4.21042
maximum branching degree: 6
time: 0.29
Initial = (0, 0, 0, all_empty),
Path = [ (0, 0, 0, all_empty), (0, 250, 0, fill(m1)), (0, 0, 250, transfer(m1, m2)), (0, 250, 250, fill(m1)), (0, 0, 500, transfer(m1, m2)), (0, 250, 500, fill(...)), (0, 200, ..., ...), (200, ..., ...)] .

true.
-->

```logtalk
performance::init, salt(100, 250, 550)::initial_state(Initial), breadth_first(11)::solve(salt(100, 250, 550), Initial, Path), salt(100, 250, 550)::print_path(Path), performance::report.
```

<!--
(0, 0, 0)	all_empty
(0, 0, 550)	fill(m2)
(0, 250, 300)	transfer(m2, m1)
(0, 0, 300)	empty(m1)
(0, 250, 50)	transfer(m2, m1)
(50, 250, 0)	transfer(m2, acc)
(50, 0, 0)	empty(m1)
(50, 0, 550)	fill(m2)
(50, 250, 300)	transfer(m2, m1)
(50, 0, 300)	empty(m1)
(50, 250, 50)	transfer(m2, m1)
(100, 250, 0)	transfer(m2, acc)
solution length: 12
state transitions (including previous solutions): 189914
ratio solution length / state transitions: 6.31865e-05
minimum branching degree: 1
average branching degree: 4.47592
maximum branching degree: 6
time: 94.44
Initial = (0, 0, 0, all_empty),
Path = [ (0, 0, 0, all_empty), (0, 0, 550, fill(m2)), (0, 250, 300, transfer(m2, m1)), (0, 0, 300, empty(m1)), (0, 250, 50, transfer(m2, m1)), (50, 250, 0, transfer(..., ...)), (50, 0, ..., ...), (50, ..., ...), (..., ...)|...] .

true.
-->

Eight puzzle solved using a hill-climbing strategy:

```logtalk
performance::init, eight_puzzle::initial_state(five_steps, Initial), hill_climbing(25)::solve(eight_puzzle, Initial, Path, Cost), eight_puzzle::print_path(Path), performance::report.
```

<!--
283
164
7 5

283
1 4
765

2 3
184
765

 23
184
765

123
 84
765

123
8 4
765
solution length: 6
number of state transitions: 15
ratio solution length / state transitions: 0.4
minimum branching degree: 2
average branching degree: 3.13333
maximum branching degree: 4
time: 0.01

Cost = 5,
Path = [[2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3],[2/2,1/2,1/3,3/3,3/2,3/1,2/1,1/1,2/3],[2/3,1/2,1/3,3/3,3/2,3/1,2/1,1/1,2/2],[1/3,1/2,2/3,3/3,3/2,3/1,2/1,1/1,2/2],[1/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,2/2],[2/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,1/2]],
Initial = [2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3] ? 

true.
-->

Eight puzzle solved using a best-first strategy:

```logtalk
performance::init, eight_puzzle::initial_state(five_steps, Initial), best_first(25)::solve(eight_puzzle, Initial, Path, Cost), eight_puzzle::print_path(Path), performance::report.
```

<!--
283
164
7 5

283
1 4
765

2 3
184
765

 23
184
765

123
 84
765

123
8 4
765
solution length: 6
number of state transitions: 15
ratio solution length / state transitions: 0.4
minimum branching degree: 2
average branching degree: 3.13333
maximum branching degree: 4
time: 0.02

Cost = 5,
Path = [[2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3],[2/2,1/2,1/3,3/3,3/2,3/1,2/1,1/1,2/3],[2/3,1/2,1/3,3/3,3/2,3/1,2/1,1/1,2/2],[1/3,1/2,2/3,3/3,3/2,3/1,2/1,1/1,2/2],[1/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,2/2],[2/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,1/2]],
Initial = [2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3] ? 

true.
-->

Turn off performance monitor

```logtalk
performance::stop.
```

<!--
true.
-->
