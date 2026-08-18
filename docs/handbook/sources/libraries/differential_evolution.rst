.. _library_differential_evolution:

``differential_evolution``
==========================

Differential Evolution (DE) is a population-based meta-heuristic for
approximating the global optimum of a continuous function. This library
implements the classic DE/rand/1/bin, DE/rand/1/exp, DE/best/1/bin, and
DE/current-to-best/1/bin strategies for bounded minimization and
maximization problems.

The library provides the parametric object
``differential_evolution(Problem, RandomAlgorithm)``, where ``Problem``
is an object implementing the
``differential_evolution_problem_protocol`` protocol and
``RandomAlgorithm`` is one of the algorithms supported by the
``fast_random`` library. A convenience object
``differential_evolution(Problem)`` uses the Xoshiro128++ random number
generator (``xoshiro128pp``).

API documentation
-----------------

Open the
`../../docs/library_index.html#differential-evolution <../../docs/library_index.html#differential-evolution>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(differential_evolution(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(differential_evolution(tester)).

Algorithm
---------

Differential Evolution maintains a population of real-valued vectors. In
every generation each target vector (x_i) produces a trial vector by
mutation followed by binomial crossover. The trial replaces the target
when it has equal or better fitness (according to the selected
objective).

Supported mutation strategies (selected with the ``strategy/1`` option):

- ``rand/1/bin`` (default)

  ::

       v = x_r1 + F * (x_r2 - x_r3)

- ``best/1/bin``

  ::

       v = x_best + F * (x_r1 - x_r2)

- ``current-to-best/1/bin``

  ::

       v = x_i + F * (x_best - x_i) + F * (x_r1 - x_r2)

Binomial crossover mixes the mutant with the target; at least one
component is always taken from the mutant. Trial vectors that leave the
box constraints are clamped to the nearest bound.

Convergence speed comparison
----------------------------

Typical behaviour on continuous problems (same population size and
control parameters):

+---------------------------+-------------+-------------+-------------+-----------------+
| Strategy                  | Convergence | Exploration | Risk of     | Best suited for |
|                           | speed       |             | premature   |                 |
|                           |             |             | convergence |                 |
+===========================+=============+=============+=============+=================+
| ``rand/1/bin``            | Slowest     | Highest     | Lowest      | Multimodal,     |
|                           |             |             |             | noisy, or       |
|                           |             |             |             | unknown         |
|                           |             |             |             | landscapes      |
+---------------------------+-------------+-------------+-------------+-----------------+
| ``rand/1/exp``            | Slowest     | Highest     | Lowest      | Multimodal,     |
|                           |             |             |             | noisy, or       |
|                           |             |             |             | unknown         |
|                           |             |             |             | landscapes      |
+---------------------------+-------------+-------------+-------------+-----------------+
| ``current-to-best/1/bin`` | Medium-Fast | Medium      | Medium      | General-purpose |
|                           |             |             |             | continuous      |
|                           |             |             |             | problems        |
+---------------------------+-------------+-------------+-------------+-----------------+
| ``best/1/bin``            | Fastest     | Lowest      | Highest     | Unimodal or     |
|                           |             |             |             | mildly          |
|                           |             |             |             | multimodal      |
|                           |             |             |             | problems        |
+---------------------------+-------------+-------------+-------------+-----------------+

- ``rand/1/bin`` and ``rand/1/exp`` share the same mutation operator and
  therefore the same exploration/exploitation balance; they differ only
  in the crossover mechanism (binomial vs. exponential).
- ``best/1/bin`` converges quickly but can stagnate on local optima.
- ``current-to-best/1/bin`` is usually the best practical compromise and
  a good default for many engineering problems.

More exploitative strategies often benefit from a modestly larger
population or a slightly smaller differential weight (``F`` in 0.5-0.7).

Defining a problem
------------------

Problem objects must implement
``differential_evolution_problem_protocol`` by defining:

- ``position_bounds(-Bounds)`` - returns one numeric ``Lower-Upper``
  pair per dimension, with ``Lower =< Upper``.
- ``fitness(+Position, -Fitness)`` - computes a numeric fitness value.
  The optimizer minimizes this value by default; use
  ``objective(maximize)`` to maximize it instead.

Optionally, a problem object may also define:

- ``initial_positions(-Positions)`` - returns a non-empty list of
  initial positions lying inside the bounds. When not defined, the
  algorithm generates a random initial population.
- ``stop_condition(+Generation, +BestPosition, +BestFitness)`` -
  succeeds when the search should terminate early.
- ``progress(+Generation, +BestPosition, +BestFitness, +MeanFitness, +Diversity)``

  - called periodically during optimization. Diversity is the mean
    Euclidean distance of population members from the centroid. A final
    report is produced when progress reporting is enabled.

Options
-------

Options for the ``run/3-4`` predicates:

- ``strategy(Strategy)`` - mutation strategy: ``rand/1/bin`` (default),
  ``best/1/bin``, or ``current-to-best/1/bin``.
- ``objective(Objective)`` - optimization direction, either ``minimize``
  or ``maximize`` (default: ``minimize``).
- ``target_fitness(Fitness)`` - numeric target that stops the run when
  the best fitness reaches or passes it in the selected objective
  direction. Use ``none`` to disable target stopping (default:
  ``none``).
- ``population_size(N)`` - population size; must be at least 4 (default:
  ``30``).
- ``max_generations(N)`` - maximum number of generations (default:
  ``100``).
- ``crossover_probability(CR)`` - binomial crossover probability in
  ``[0,1]`` (default: ``0.9``).
- ``differential_weight(F)`` - positive scale factor for difference
  vectors (default: ``0.8``).
- ``stagnation_generations(N)`` - number of consecutive generations
  without a strict best-fitness improvement before stopping. Set to
  ``0`` to disable (default: ``0``).
- ``updates(N)`` - number of progress reports during the run. Set to
  ``0`` to disable progress reporting (default: ``0``).
- ``seed(S)`` - positive integer random seed for reproducible runs.

Run statistics
--------------

The ``run/4`` predicate returns:

- ``generations(N)`` - number of completed generations.
- ``evaluations(E)`` - number of fitness evaluations, including the
  initial population.
- ``improvements(I)`` - number of generations that improved the global
  best.
- ``final_mean_fitness(M)`` - mean fitness of the final population.
- ``final_diversity(D)`` - diversity of the final population.

Fitness values are reported unchanged. The ``objective/1`` option only
controls which values are considered better.

Limitations
-----------

- Continuous numeric vectors only (no discrete, integer, or
  mixed-integer variables).
- Box constraints only. Trial vectors that leave the bounds are clamped;
  there is no support for general equality/inequality constraints or
  penalty/barrier methods.
- Static control parameters. The differential weight (``F``) and
  crossover probability (``CR``) are fixed for the whole run;
  self-adaptive variants (jDE, SHADE, L-SHADE, ...) are not yet
  implemented.
- Only four mutation strategies are provided: ``rand/1/bin``
  ``rand/1/exp``,
- ``best/1/bin``, and ``current-to-best/1/bin``. Other classic variants
  (``rand/2``, ``best/2``, exponential crossover, ...) are currently
  absent.
- Synchronous generational replacement only.
- No automatic restarts, diversity preservation, or stagnation-triggered
  population re-initialization beyond the simple
  ``stagnation_generations/1`` stopping criterion.
- Single-objective optimization only.

Usage
-----

.. _defining-a-problem-1:

Defining a problem
~~~~~~~~~~~~~~~~~~

::

   :- object(sphere,
       implements(differential_evolution_problem_protocol)).

       position_bounds([(-5.0)-5.0, (-5.0)-5.0]).

       fitness([X, Y], Fitness) :-
           Fitness is X*X + Y*Y.

   :- end_object.

Running with the default strategy (rand/1/bin)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- differential_evolution(sphere)::run(Position, Fitness).

Selecting a strategy
~~~~~~~~~~~~~~~~~~~~

::

   | ?- differential_evolution(sphere)::run(
            Position, Fitness,
            [strategy(current-to-best/1/bin), seed(42)]
        ).

Reproducible run with statistics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- differential_evolution(sphere)::run(
            Position, Fitness, Statistics,
            [seed(42), max_generations(200), population_size(40)]
        ).

Maximization
~~~~~~~~~~~~

::

   | ?- differential_evolution(Problem)::run(
            Position, Fitness,
            [objective(maximize), strategy(best/1/bin), seed(42)]
        ).

Stopping after stagnation
~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- differential_evolution(Problem)::run(
            Position, Fitness, Statistics,
            [stagnation_generations(25), seed(42)]
        ).

Using a custom random number generator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- differential_evolution(sphere, well512a)::run(
            Position, Fitness, [seed(42)]
        ).
