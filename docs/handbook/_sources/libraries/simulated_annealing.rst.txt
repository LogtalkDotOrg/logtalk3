.. _library_simulated_annealing:

``simulated_annealing``
=======================

Simulated annealing is a probabilistic meta-heuristic for approximating
the global optimum of a given function. It is particularly useful for
combinatorial optimization problems such as the Traveling Salesman
Problem (TSP), graph coloring, and scheduling.

The library provides the parametric object
``simulated_annealing(Problem, RandomAlgorithm)`` where ``Problem`` is
an object implementing the ``simulated_annealing_protocol`` protocol and
``RandomAlgorithm`` is one of the algorithms supported by the
``fast_random`` library. The algorithm minimizes the energy (cost)
function defined by the problem.

A convenience object ``simulated_annealing(Problem)`` is also provided,
using the Xoshiro128++ random number generator (``xoshiro128pp``) as the
default.

API documentation
-----------------

Open the
`../../docs/library_index.html#simulated-annealing <../../docs/library_index.html#simulated-annealing>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(simulated_annealing(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(simulated_annealing(tester)).

Features
--------

- **Configurable random number generator** — the algorithm is
  parameterized by a ``fast_random`` algorithm. Available algorithms
  include ``xoshiro128pp``, ``xoshiro128ss``, ``xoshiro256pp``,
  ``xoshiro256ss``, ``well512a``, ``splitmix64``, and ``as183``. The
  convenience object ``simulated_annealing(Problem)`` defaults to
  ``xoshiro128pp``.
- **Boltzmann acceptance criterion** — a worse neighbor is accepted with
  probability ``exp(-DeltaE / Temperature)``, allowing the search to
  escape local minima early in the run while converging as the
  temperature cools.
- **Configurable cooling schedule** — default geometric cooling
  (``NewTemp is Temp * 0.995``), overridable by defining
  ``cooling_schedule/3`` in the problem object.
- **Custom stop conditions** — the search stops when the maximum number
  of steps is reached or the temperature drops below the minimum
  temperature. A problem object can define ``stop_condition/3`` to
  terminate early (e.g. when a good-enough solution is found).
- **Delta-energy optimization** — when the problem object defines
  ``neighbor_state/3``, the algorithm uses the returned energy delta
  directly instead of recomputing the full energy. This is useful when
  computing the change is cheaper than evaluating the full cost (e.g. a
  2-opt swap in TSP).
- **Best state tracking** — the algorithm tracks the best state found
  across all iterations and across all restart cycles, not just the
  final state.
- **Progress reporting** — if the problem object defines ``progress/5``,
  it is called periodically with the current step, temperature, best
  energy, acceptance rate, and improvement rate. A final report is
  always produced when the loop terminates.
- **Run statistics** — the ``run/4`` predicate returns a list of
  statistics including the number of steps, acceptances, improvements,
  and the final temperature.
- **Seed control** — the ``seed(S)`` option initializes the random
  number generator for reproducible runs.
- **Reheating restarts** — the ``restarts(N)`` option runs N additional
  SA cycles after the first. Each restart reheats the temperature to the
  initial value and begins from the best state found so far, allowing
  the search to escape deep local minima. Statistics accumulate across
  all cycles.
- **Auto-temperature estimation** — the ``estimate_temperature/1-2``
  predicates sample random neighbor transitions and compute an initial
  temperature that would produce a target acceptance rate, avoiding
  manual tuning.

Defining a problem
------------------

A problem object must implement the ``simulated_annealing_protocol``
protocol by defining (at least) the following four predicates:

- ``initial_state(-State)`` — returns the starting state.
- ``neighbor_state(+State, -Neighbor)`` — generates a neighboring state.
- ``state_energy(+State, -Energy)`` — computes the cost of a state (to
  be minimized).
- ``initial_temperature(-Temperature)`` — returns the starting
  temperature.

Optionally, the problem object may also define:

- ``neighbor_state(+State, -Neighbor, -DeltaEnergy)`` — generates a
  neighboring state and returns the energy change directly, avoiding a
  full energy recomputation. Useful when computing the delta is cheaper
  than recomputing the full energy (e.g. TSP with a 2-opt swap).
- ``cooling_schedule(+Temperature, +Step, -NewTemperature)`` — computes
  the next temperature. Default: geometric cooling
  (``NewTemp is Temp * 0.995``).
- ``stop_condition(+Step, +Temperature, +BestEnergy)`` — succeeds when
  the search should terminate early (e.g. when a good-enough solution is
  found). Default: the search runs until the maximum number of steps is
  reached or the temperature drops below the minimum temperature.
- ``progress(+Step, +Temperature, +BestEnergy, +AcceptanceRate, +ImprovementRate)``
  — called periodically during the optimization to report progress. A
  final report is always produced when the loop terminates. The
  acceptance and improvement rates are floats between 0.0 and 1.0.

Options
-------

Options for the ``run/3-4`` predicates:

- ``max_steps(N)`` — maximum number of iterations per cycle (default:
  ``10000``).
- ``min_temperature(T)`` — minimum temperature floor; the search stops
  when the temperature drops below this value (default: ``0.001``).
- ``updates(N)`` — number of progress reports during the run. Progress
  is reported by calling ``progress/5`` on the problem object. Set to
  ``0`` to disable (default: ``0``).
- ``seed(S)`` — positive integer seed for the random number generator,
  enabling reproducible runs (default: none).
- ``restarts(N)`` — number of additional SA cycles after the first. Each
  restart reheats the temperature to the initial value and begins from
  the best state found so far, allowing the search to escape local
  minima (default: ``0``).

Options for the ``estimate_temperature/2`` predicate:

- ``samples(N)`` — number of random neighbor transitions to sample
  (default: ``200``).
- ``acceptance_rate(P)`` — target initial acceptance rate as an integer
  percentage between 1 and 99 (default: ``80``).

Run statistics
--------------

The ``run/4`` predicate returns a list of statistics about the completed
run:

- ``steps(N)`` — total number of steps executed.
- ``acceptances(A)`` — number of accepted moves (both improving and
  uphill).
- ``improvements(I)`` — number of moves that strictly improved the best
  energy found.
- ``final_temperature(T)`` — temperature at termination.

Usage
-----

.. _defining-a-problem-1:

Defining a problem
~~~~~~~~~~~~~~~~~~

Define an object implementing the ``simulated_annealing_protocol``
protocol. For example, a simple quadratic minimization problem:

::

   :- object(quadratic,
       implements(simulated_annealing_protocol)).

       initial_state(50.0).
       neighbor_state(X, Y) :-
           random::random(-5.0, 5.0, Delta),
           Y is X + Delta.
       state_energy(X, E) :-
           E is (X - 3.0) * (X - 3.0).
       initial_temperature(100.0).

   :- end_object.

Running the algorithm
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- simulated_annealing(quadratic)::run(State, Energy).
   State = 3.001..., Energy = 0.000...

Running with custom options
~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- simulated_annealing(quadratic)::run(State, Energy, [max_steps(50000)]).
   State = 3.000..., Energy = 0.000...

Running with statistics
~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- simulated_annealing(quadratic)::run(State, Energy, Stats, []).
   State = 3.001..., Energy = 0.000...,
   Stats = [steps(10000), acceptances(...), improvements(...), final_temperature(...)]

Reproducible runs with seed
~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- simulated_annealing(quadratic)::run(S1, E1, [seed(42)]),
        simulated_annealing(quadratic)::run(S2, E2, [seed(42)]).
   S1 = S2, E1 = E2.

Reheating restarts
~~~~~~~~~~~~~~~~~~

Run 3 SA cycles (1 initial + 2 restarts). Each restart reheats the
temperature and begins from the best state found so far:

::

   | ?- simulated_annealing(quadratic)::run(State, Energy, [restarts(2)]).
   State = 3.000..., Energy = 0.000...

Auto-temperature estimation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Estimate a good initial temperature for the problem by sampling random
neighbor transitions:

::

   | ?- simulated_annealing(quadratic)::estimate_temperature(T).
   T = ...

With custom parameters (500 samples, 90% target acceptance rate):

::

   | ?- simulated_annealing(quadratic)::estimate_temperature(T, [samples(500), acceptance_rate(90)]).
   T = ...

Using a custom random number generator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the two-parameter version to select a specific ``fast_random``
algorithm:

::

   | ?- simulated_annealing(quadratic, well512a)::run(State, Energy).
   State = 3.001..., Energy = 0.000...

   | ?- simulated_annealing(quadratic, xoshiro256ss)::run(State, Energy, [seed(42)]).
   State = 3.000..., Energy = 0.000...
