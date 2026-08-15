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


`tabu_search`
=============

Tabu search is a metaheuristic that guides a local-search procedure by
using a short-term memory structure (the *tabu list*) to avoid revisiting
recently explored solutions and to escape local minima. It is particularly
useful for combinatorial optimization problems such as the Traveling
Salesman Problem (TSP), graph coloring, and scheduling.

The library provides the parametric object `tabu_search(Problem, RandomAlgorithm)`
where `Problem` is an object implementing the `tabu_search_problem_protocol`
protocol and `RandomAlgorithm` is one of the algorithms supported by the
`fast_random` library. The algorithm minimizes the energy (cost) function
defined by the problem.

A convenience object `tabu_search(Problem)` is also provided, using
the Xoshiro128++ random number generator (`xoshiro128pp`) as the default.


API documentation
-----------------

Open the [../../docs/library_index.html#tabu-search](../../docs/library_index.html#tabu-search)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(tabu_search(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(tabu_search(tester)).


Features
--------

- **Configurable random number generator** — the algorithm is parameterized
  by a `fast_random` algorithm. Available algorithms include `xoshiro128pp`,
  `xoshiro128ss`, `xoshiro256pp`, `xoshiro256ss`, `well512a`, `splitmix64`,
  and `as183`. The convenience object `tabu_search(Problem)` defaults
  to `xoshiro128pp`.
- **Tabu list (short-term memory)** — a FIFO list of recently visited states
  of configurable maximum length (`tabu_tenure`). Candidates that appear in
  the list are forbidden unless the aspiration criterion is met.
- **Aspiration criterion** — a tabu candidate is accepted when its energy is
  strictly better than the best energy found so far (classic “best-so-far”
  aspiration).
- **Candidate sampling** — by default the algorithm samples `candidates(N)`
  neighbors per iteration using `neighbor_state/2` (or `neighbor_state/3`).
  If the problem defines `neighbors/2`, that complete list is used (or a
  random sample of it when larger than the candidate limit).
- **Delta-energy optimization** — when the problem object defines
  `neighbor_state/3`, the algorithm uses the returned energy delta directly
  instead of recomputing the full energy.
- **Best state tracking** — the algorithm tracks the best state found across
  all iterations and across all restart cycles, not just the final state.
- **Progress reporting** — if the problem object defines `progress/5`, it is
  called periodically with the current step, best energy, current energy,
  acceptance rate, and improvement rate. A final report is always produced
  when the loop terminates.
- **Run statistics** — the `run/4` predicate returns a list of statistics
  including the number of steps, acceptances, improvements, and the final
  tabu-list size.
- **Seed control** — the `seed(S)` option initializes the random number
  generator for reproducible runs.
- **Restarts** — the `restarts(N)` option runs N additional tabu-search
  cycles after the first. Each restart begins from the best state found so
  far with a cleared tabu list, allowing the search to escape deep local
  minima. Statistics accumulate across all cycles.


Defining a problem
------------------

A problem object must implement the `tabu_search_problem_protocol` protocol
by defining (at least) the following three predicates:

- `initial_state(-State)` — returns the starting state.
- `neighbor_state(+State, -Neighbor)` — generates a neighboring state.
- `state_energy(+State, -Energy)` — computes the cost of a state (to be
  minimized).

Optionally, the problem object may also define:

- `neighbor_state(+State, -Neighbor, -DeltaEnergy)` — generates a
  neighboring state and returns the energy change directly, avoiding a full
  energy recomputation.
- `neighbors(+State, -Neighbors)` — returns the complete list of neighboring
  states. When defined, the algorithm uses this list (or a random sample
  controlled by `candidates(N)`) instead of repeated calls to
  `neighbor_state/2`.
- `stop_condition(+Step, +BestEnergy, +CurrentEnergy)` — succeeds when the
  search should terminate early.
- `progress(+Step, +BestEnergy, +CurrentEnergy, +AcceptanceRate, +ImprovementRate)`
  — called periodically during the optimization to report progress.


Options
-------

Options for the `run/3-4` predicates:

- `max_steps(N)` — maximum number of iterations per cycle (default: `10000`).
- `tabu_tenure(T)` — maximum length of the tabu list (default: `7`).
- `candidates(N)` — number of candidate neighbors examined per iteration
  (default: `20`).
- `updates(N)` — number of progress reports during the run. Progress is
  reported by calling `progress/5` on the problem object. Set to `0` to
  disable (default: `0`).
- `seed(S)` — positive integer seed for the random number generator,
  enabling reproducible runs (default: none).
- `restarts(N)` — number of additional tabu-search cycles after the first.
  Each restart begins from the best state found so far with a cleared tabu
  list (default: `0`).


Run statistics
--------------

The `run/4` predicate returns a list of statistics about the completed
run:

- `steps(N)` — total number of steps executed.
- `acceptances(A)` — number of accepted moves.
- `improvements(I)` — number of moves that strictly improved the best
  energy found.
- `final_tabu_size(S)` — size of the tabu list at termination.


**Limitations**
-------------

- **Solution-based tabu memory only** — the tabu list stores complete states
  rather than move attributes. This is simple and problem-agnostic, but can
  be memory-intensive for large or richly structured states, and is often
  less effective than attribute-based tabu for problems where the relevant
  forbidden features are local (e.g. edges in TSP, variable assignments in
  scheduling).

- **Fixed tabu tenure** — the tenure is a constant given by the `tabu_tenure`
  option. There is no reactive, random, or adaptive tenure schedule.

- **Single aspiration criterion** — only the classic "best-so-far" rule is
  implemented (a tabu candidate is accepted when it improves the global best
  energy). Other aspiration strategies are not supported.

- **Neighborhood exploration** — when the problem does not define
  `neighbors/2`, the algorithm samples a fixed number of candidates via
  repeated calls to `neighbor_state/2`. Systematic or exhaustive enumeration
  of large neighborhoods is left to the problem object.

- **Short-term memory only** — there is no intermediate-term or long-term
  memory (frequency-based or elite-set structures) and therefore no built-in
  intensification or diversification beyond the optional `restarts(N)`
  mechanism, which simply clears the tabu list and resumes from the best
  state found so far.

- **State identity** — tabu membership is decided by ordinary term equality
  (`member/2`). States that are semantically equivalent but not term-identical
  (e.g. rotations or reflections of a tour) are treated as distinct unless
  the problem normalizes them.

- **Single trajectory** — the search follows one solution path at a time;
  population-based or multi-threaded variants are not currently implemented.


Usage
-----

### Defining a problem

Define an object implementing the `tabu_search_problem_protocol` protocol.
For example, a simple quadratic minimization problem:

	:- object(quadratic,
		implements(tabu_search_problem_protocol)).

		initial_state(50.0).
		neighbor_state(X, Y) :-
			random::random(-5.0, 5.0, Delta),
			Y is X + Delta.
		state_energy(X, E) :-
			E is (X - 3.0) * (X - 3.0).

	:- end_object.

### Running the algorithm

	| ?- tabu_search(quadratic)::run(State, Energy).
	State = 3.00..., Energy = 0.000...

### Running with custom options

	| ?- tabu_search(quadratic)::run(State, Energy, [max_steps(5000), tabu_tenure(10), candidates(30)]).
	State = 3.00..., Energy = 0.000...

### Running with statistics

	| ?- tabu_search(quadratic)::run(State, Energy, Stats, []).
	State = 3.00..., Energy = 0.000...,
	Stats = [steps(10000), acceptances(...), improvements(...), final_tabu_size(...)]

### Reproducible runs with seed

	| ?- tabu_search(quadratic)::run(S1, E1, [seed(42)]),
	     tabu_search(quadratic)::run(S2, E2, [seed(42)]).
	S1 = S2, E1 = E2.

### Restarts

Run 3 tabu-search cycles (1 initial + 2 restarts). Each restart clears the
tabu list and begins from the best state found so far:

	| ?- tabu_search(quadratic)::run(State, Energy, [restarts(2)]).
	State = 3.00..., Energy = 0.000...

### Using a custom random number generator

Use the two-parameter version to select a specific `fast_random` algorithm:

	| ?- tabu_search(quadratic, well512a)::run(State, Energy).
	State = 3.00..., Energy = 0.000...

	| ?- tabu_search(quadratic, xoshiro256ss)::run(State, Energy, [seed(42)]).
	State = 3.00..., Energy = 0.000...
