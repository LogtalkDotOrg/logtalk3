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


`genetic_algorithm`
===================

Genetic algorithms are population-based meta-heuristics inspired by natural
selection. A population of candidate solutions (individuals) evolves over
successive generations through selection, crossover (recombination), and
mutation. They are effective for combinatorial and continuous optimization
problems such as the Traveling Salesman Problem (TSP), scheduling, and
parameter tuning.

The library provides the parametric object
`genetic_algorithm(Problem, RandomAlgorithm)` where `Problem` is an object
implementing the `genetic_algorithm_problem_protocol` protocol and
`RandomAlgorithm` is one of the algorithms supported by the `fast_random`
library. The algorithm minimizes the energy (cost) function defined by the
problem by default; maximization is supported via the `objective/1` option.

A convenience object `genetic_algorithm(Problem)` is also provided, using
the Xoshiro128++ random number generator (`xoshiro128pp`) as the default.


API documentation
-----------------

Open the [../../docs/library_index.html#genetic-algorithm](../../docs/library_index.html#genetic-algorithm)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(genetic_algorithm(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(genetic_algorithm(tester)).


Features
--------

- **Configurable random number generator** — the algorithm is parameterized
  by a `fast_random` algorithm. Available algorithms include `xoshiro128pp`,
  `xoshiro128ss`, `xoshiro256pp`, `xoshiro256ss`, `well512a`, `splitmix64`,
  and `as183`. The convenience object `genetic_algorithm(Problem)` defaults
  to `xoshiro128pp`.
- **Configurable selection** — parent selection is controlled by the
  `selection/1` option. Supported schemes are `tournament(K)` (default
  `tournament(3)`), `roulette` (fitness-proportionate), and `rank` (linear
  rank weights after sorting by objective).
- **Adaptive rates** — crossover and mutation rates can be held constant or
  adapted each generation. Built-in schedules are `constant`,
  `linear(Initial, Final)`, and `geometric(Factor)`. Optional problem hooks
  `crossover_rate/4` and `mutation_rate/4` override the configured schedules
  when defined.
- **Problem-defined genetic operators** — crossover (`crossover/4`) and
  mutation (`mutate/2`) are supplied by the problem object, allowing
  arbitrary representations (permutations, real vectors, bit-strings, trees,
  etc.).
- **Elitism preservation** — the best `elite_size(N)` individuals are copied
  unchanged into the next generation (default N = 1), protecting them from
  crossover and mutation. N is clamped to the population size. Set to 0 to
  disable.
- **Minimization or maximization** — controlled by the `objective/1` option
  (`minimize` by default).
- **Best individual tracking** — the algorithm tracks the best individual
  found across all generations, not just the final population.
- **Progress reporting** — if the problem object defines `progress/5`, it is
  called periodically with the current generation, best individual, best
  energy, mean population energy, and diversity. A final report is always
  produced when updates are enabled.
- **Run statistics** — the `run/4` predicate returns a list of statistics
  including the number of generations, fitness evaluations, improvements,
  and the final population size.
- **Seed control** — the `seed(S)` option initializes the random number
  generator for reproducible runs.
- **Optional initial population** — when the problem defines
  `initial_population/1` that list is used (padded or truncated to the
  requested size); otherwise individuals are generated via
  `random_individual/1`.


Defining a problem
------------------

A problem object must implement the `genetic_algorithm_problem_protocol`
protocol by defining (at least) the following predicates:

- `random_individual(-Individual)` — generates a random candidate solution.
- `state_energy(+Individual, -Energy)` — computes the cost/fitness of an
  individual (minimized by default).
- `crossover(+Parent1, +Parent2, -Offspring1, -Offspring2)` — recombines two
  parents into two offspring.
- `mutate(+Individual, -Mutated)` — produces a mutated individual.

Optionally, the problem object may also define:

- `initial_population(-Population)` — returns a non-empty list of initial
  individuals.
- `stop_condition(+Generation, +BestIndividual, +BestEnergy)` — succeeds
  when the search should terminate early.
- `progress(+Generation, +BestIndividual, +BestEnergy, +MeanEnergy, +Diversity)`
  — called periodically to report progress.
- `diversity(+Population, -Diversity)` — computes a numeric diversity
  measure for progress reporting.
- `crossover_rate(+Generation, +MaxGenerations, +CurrentRate, -NewRate)` —
  optional adaptive crossover-rate hook (overrides `crossover_schedule/1`).
- `mutation_rate(+Generation, +MaxGenerations, +CurrentRate, -NewRate)` —
  optional adaptive mutation-rate hook (overrides `mutation_schedule/1`).


Options
-------

Options for the `run/3-4` predicates:

- `max_generations(N)` — maximum number of generations (default: `200`).
- `population_size(N)` — population size; must be at least 2 (default: `50`).
- `crossover_rate(P)` — initial crossover probability (default: `0.8`).
- `mutation_rate(P)` — initial mutation probability (default: `0.1`).
- `crossover_schedule(Schedule)` — `constant` (default),
  `linear(Initial, Final)`, or `geometric(Factor)`. Overridden by a problem
  `crossover_rate/4` hook when defined.
- `mutation_schedule(Schedule)` — `constant` (default),
  `linear(Initial, Final)`, or `geometric(Factor)`. Overridden by a problem
  `mutation_rate/4` hook when defined.
- `selection(Scheme)` — parent selection scheme: `tournament(K)` with
  positive integer K (default: `tournament(3)`), `roulette`, or `rank`.
- `elite_size(N)` — number of elite individuals preserved each generation
  (default: `1`). Use `0` to disable elitism.
- `objective(Direction)` — `minimize` (default) or `maximize`.
- `updates(N)` — number of progress reports during the run. Set to `0` to
  disable (default: `0`).
- `seed(S)` — positive integer seed for the random number generator,
  enabling reproducible runs (default: none).


Run statistics
--------------

The `run/4` predicate returns a list of statistics about the completed
run:

- `generations(N)` — number of generations executed.
- `evaluations(E)` — total number of fitness evaluations.
- `improvements(I)` — number of generations that strictly improved the best
  energy found.
- `final_population_size(S)` — size of the final population.


**Limitations**
-------------

- **Single population, generational model** — a classic generational GA is
  implemented. Steady-state replacement, multi-population (island) models,
  and coevolution are not provided.
- **Limited selection schemes** — tournament, roulette-wheel, and linear
  rank selection are implemented. Other schemes (e.g. stochastic universal
  sampling, Boltzmann selection) are not provided.
- **Limited adaptive schedules** — constant, linear, and geometric rate
  schedules are supported, plus optional problem hooks. Other adaptive
  policies (e.g. diversity-triggered rates) require a custom hook.
- **No built-in representation** — chromosomes, real vectors, and other
  encodings must be defined by the problem together with suitable crossover
  and mutation operators.
- **Diversity is optional** — a problem-specific `diversity/2` predicate is
  required for meaningful diversity reporting; otherwise a placeholder
  value of 0.0 is used.


Usage
-----

### Defining a problem

Define an object implementing the `genetic_algorithm_problem_protocol`
protocol. For example, a simple quadratic minimization problem on the real
line:

	:- object(quadratic,
		implements(genetic_algorithm_problem_protocol)).

		:- uses(fast_random(xoshiro128pp), [
			random/3, randomize/1
		]).

		:- public(reset_seed/0).
		reset_seed :-
			randomize(12345).

		random_individual(X) :-
			random(-50.0, 50.0, X).

		state_energy(X, E) :-
			E is (X - 3.0) * (X - 3.0).

		crossover(X, Y, C1, C2) :-
			% arithmetic crossover
			Alpha is 0.5,
			C1 is Alpha*X + (1-Alpha)*Y,
			C2 is Alpha*Y + (1-Alpha)*X.

		mutate(X, Y) :-
			random(-2.0, 2.0, Delta),
			Y is X + Delta.

	:- end_object.

### Running the algorithm

	| ?- genetic_algorithm(quadratic)::run(State, Energy).
	State = 3.00..., Energy = 0.000...

### Running with custom options

	| ?- genetic_algorithm(quadratic)::run(State, Energy, [
	         max_generations(100),
	         population_size(40),
	         crossover_rate(0.9),
	         mutation_rate(0.15),
	         elite_size(2)
	     ]).
	State = 3.00..., Energy = 0.000...

### Running with statistics

	| ?- genetic_algorithm(quadratic)::run(State, Energy, Stats, []).
	State = 3.00..., Energy = 0.000...,
	Stats = [generations(200), evaluations(...), improvements(...), final_population_size(50)]

### Reproducible runs with seed

	| ?- genetic_algorithm(quadratic)::run(S1, E1, [seed(42)]),
	     genetic_algorithm(quadratic)::run(S2, E2, [seed(42)]).
	S1 = S2, E1 = E2.

### Maximization

	| ?- genetic_algorithm(quadratic)::run(State, Energy, [objective(maximize)]).
	% (would climb away from the minimum if the landscape allowed)

### Using a custom random number generator

	| ?- genetic_algorithm(quadratic, well512a)::run(State, Energy).
	State = 3.00..., Energy = 0.000...
