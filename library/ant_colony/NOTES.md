________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


`ant_colony`
============

Ant Colony Optimization (ACO) is a constructive metaheuristic inspired by
the foraging behaviour of real ants. Artificial ants build candidate
solutions component-by-component on a construction graph, guided by
pheromone trails and problem-specific heuristic information. After each
iteration the pheromone is evaporated and reinforced according to the
quality of the constructed solutions. ACO is particularly effective for
combinatorial problems that can be stated as finding good paths or
permutations on a graph, such as the Traveling Salesman Problem (TSP),
vehicle routing, and scheduling.

The library provides the parametric object `ant_colony(Problem, RandomAlgorithm)`
where `Problem` is an object implementing the `ant_colony_problem_protocol`
protocol and `RandomAlgorithm` is one of the algorithms supported by the
`fast_random` library. The algorithm minimizes the solution cost defined
by the problem.

A convenience object `ant_colony(Problem)` is also provided, using the
Xoshiro128++ random number generator (`xoshiro128pp`) as the default.


API documentation
------------------

Open the [../../docs/library_index.html#ant-colony](../../docs/library_index.html#ant-colony)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(ant_colony(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(ant_colony(tester)).


Features
--------

- **Configurable random number generator** - the algorithm is parameterized
  by a `fast_random` algorithm. Available algorithms include `xoshiro128pp`,
  `xoshiro128ss`, `xoshiro256pp`, `xoshiro256ss`, `well512a`, `splitmix64`,
  and `as183`. The convenience object `ant_colony(Problem)` defaults to
  `xoshiro128pp`.
- **Classic Ant System pheromone update** - every edge evaporates by factor
  `(1-Rho)`; each ant then deposits `Q / Cost` on the edges of its tour.
- **MAX-MIN pheromone bounds** - optional `tau_min(T)` and `tau_max(T)`
  clamp every trail into a closed interval after each update (MMAS-style),
  preventing trail collapse or domination. Defaults are a tiny positive
  floor and a very large ceiling.
- **Elitist reinforcement** - the optional `elite(E)` option adds an extra
  `E * Q / BestCost` deposit on the edges of the global-best tour.
- **Probabilistic construction** - each ant builds a complete tour by
  starting at a random node and selecting successive unvisited nodes with
  probability proportional to `Tau^Alpha * Eta^Beta` (roulette-wheel
  selection).
- **Best solution tracking** - the algorithm retains the best solution
  found across all iterations.
- **Progress reporting** - if the problem object defines `progress/5`, it is
  called periodically with the current iteration, best cost, iteration-best
  cost, and rate statistics.
- **Run statistics** - the `run/4` predicate returns the number of
  iterations, total solutions constructed, number of global-best
  improvements, and the final best cost.
- **Seed control** - the `seed(S)` option initializes the random number
  generator for reproducible runs.


Defining a problem
------------------

A problem object must implement the `ant_colony_problem_protocol` protocol
by defining (at least) the following three predicates:

- `nodes(-Nodes)` - returns the list of nodes of the construction graph.
- `heuristic(+From, +To, -Eta)` - returns the heuristic desirability of the
  directed edge from `From` to `To` (typically the reciprocal of distance
  or cost; must be strictly positive).
- `solution_cost(+Tour, -Cost)` - computes the cost of a complete tour
  (the algorithm minimizes this value). The tour is a list of nodes; the
  cost predicate is responsible for adding the closing edge back to the
  first node when required (as in TSP).

Optionally, the problem object may also define:

- `stop_condition(+Iteration, +BestCost, +IterationBestCost)` - succeeds
  when the search should terminate early.
- `progress(+Iteration, +BestCost, +IterationBestCost, +AcceptanceRate, +ImprovementRate)`
  - called periodically during the optimization to report progress.


Options
-------

Options for the `run/3-4` predicates:

- `max_iterations(N)` - maximum number of iterations (default: `100`).
- `ants(N)` - number of ants (solutions constructed) per iteration
  (default: `10`).
- `alpha(A)` - pheromone importance exponent (default: `1.0`).
- `beta(B)` - heuristic importance exponent (default: `2.0`).
- `rho(R)` - evaporation rate in `(0,1]` (default: `0.5`).
- `q(Q)` - pheromone deposit constant (default: `100.0`).
- `elite(E)` - elitist weight: extra deposit factor for the global-best
  tour (default: `0`).
- `tau0(T)` - initial pheromone level on every edge (default: `1.0`).
  Clamped into `[tau_min, tau_max]` at initialization.
- `tau_min(T)` - lower bound on pheromone trails; must be strictly
  positive (default: `1.0e-12`).
- `tau_max(T)` - upper bound on pheromone trails; must be strictly
  positive and at least `tau_min` (default: `1.0e300`).
- `updates(N)` - number of progress reports during the run. Progress is
  reported by calling `progress/5` on the problem object. Set to `0` to
  disable (default: `0`).
- `seed(S)` - positive integer seed for the random number generator,
  enabling reproducible runs (default: none).


Run statistics
--------------

The `run/4` predicate returns a list of statistics about the completed
run:

- `iterations(N)` - number of iterations executed.
- `solutions(S)` - total number of solutions constructed.
- `improvements(I)` - number of times the global best was strictly improved.
- `final_best_cost(C)` - cost of the best solution found.


**Limitations**
-------------

- **Solution-construction model only** - the library implements the classic
  constructive ACO paradigm (ants build complete solutions from
  components). Local-search improvement of constructed solutions is left
  to the problem object or a surrounding hybrid.
- **Edge-based pheromone** - pheromone is stored on directed edges of the
  complete graph induced by `nodes/1`. Attribute-based or set-based
  pheromone models are not provided.
- **Single colony, single pheromone matrix** - multi-colony and multi-objective
  variants are not implemented.
- **TSP-oriented construction** - the built-in construction procedure
  assumes a Hamiltonian path/tour over the node set (every node visited
  exactly once). Problems with different feasibility rules need a custom
  construction strategy (future extension point).


Usage
-----

### Defining a problem

Define an object implementing the `ant_colony_problem_protocol` protocol.
For example, a small TSP instance:

	:- object(tsp6,
		implements(ant_colony_problem_protocol)).

		nodes([a,b,c,d,e,f]).

		heuristic(From, To, Eta) :-
			distance(From, To, D),
			Eta is 1.0 / D.

		solution_cost(Tour, Cost) :-
			% sum of successive distances plus the closing edge
			...

	:- end_object.

### Running the algorithm

	| ?- ant_colony(tsp6)::run(Tour, Cost).
	Tour = [a,b,c,...], Cost = ...

### Running with custom options

	| ?- ant_colony(tsp6)::run(Tour, Cost, [
	         max_iterations(200), ants(20), alpha(1.0), beta(5.0),
	         rho(0.1), elite(2)
	     ]).

### Running with statistics

	| ?- ant_colony(tsp6)::run(Tour, Cost, Stats, []).
	Stats = [iterations(100), solutions(1000), improvements(...), final_best_cost(...)]

### Reproducible runs with seed

	| ?- ant_colony(tsp6)::run(T1, C1, [seed(42)]),
	     ant_colony(tsp6)::run(T2, C2, [seed(42)]).
	T1 = T2, C1 = C2.

### Using a custom random number generator

	| ?- ant_colony(tsp6, well512a)::run(Tour, Cost).
