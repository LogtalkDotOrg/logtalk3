________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

Some of the code in this folder is adapted, with permission, from the book 
"Prolog Programming for Artificial Intelligence" by Ivan Bratko.

For a description of the search problems, please see a classical AI book 
(such as the one above) or visit <http://www.plastelina.net/games>.

This example defines two hierarchies of objects, one for representing 
state-spaces and another for representing search methods:

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

Taken together, these two hierarchies implement a framework for solving 
state-space search problems in Logtalk. There is also a monitor object, 
`performance`, which tries to measure the time taken to find a solution, 
the branching factor while searching for a solution, and the number of 
transitions made to find a solution.
