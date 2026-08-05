.. _library_particle_swarm_optimization:

``particle_swarm_optimization``
===============================

Particle swarm optimization (PSO) is a population-based meta-heuristic
for approximating the global optimum of a function. This library
implements continuous, bounded, synchronous global-best PSO for
minimization and maximization problems.

The library provides the parametric object
``particle_swarm_optimization(Problem, RandomAlgorithm)``, where
``Problem`` is an object implementing the
``particle_swarm_optimization_protocol`` protocol and
``RandomAlgorithm`` is one of the algorithms supported by the
``fast_random`` library. A convenience object
``particle_swarm_optimization(Problem)`` uses the Xoshiro128++ random
number generator (``xoshiro128pp``).

API documentation
-----------------

Open the
`../../docs/library_index.html#particle-swarm-optimization <../../docs/library_index.html#particle-swarm-optimization>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(particle_swarm_optimization(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(particle_swarm_optimization(tester)).

Algorithm
---------

For every dimension of every particle, velocity and position are updated
as:

::

   V' = W*V + C1*R1*(PersonalBest-X) + C2*R2*(GlobalBest-X)
   X' = X + V'

``W`` is the inertia weight, ``C1`` and ``C2`` are the cognitive and
social coefficients, and ``R1`` and ``R2`` are independent random values
in ``[0,1)``. Updates are synchronous: all particles in an iteration use
the global best known at the start of that iteration.

By default, initial velocities are sampled uniformly from plus or minus
each dimension range. A problem may instead supply initial velocities.
Velocities remain limited to this range. When a position crosses a
bound, it is clamped to the bound and the corresponding velocity
component is set to zero.

Optionally, the run can stop after a configured number of consecutive
iterations without a strict global-best improvement. Improvement follows
the selected objective direction; equal fitness values count as
non-improvements. Stagnation stopping returns the best result found
without restarting or reinitializing the swarm.

Defining a problem
------------------

A problem object must implement the
``particle_swarm_optimization_protocol`` protocol by defining:

- ``initial_positions(-Positions)`` — returns a non-empty list of
  particle positions. Every position is a non-empty numeric list of the
  same length.
- ``position_bounds(-Bounds)`` — returns one numeric ``Lower-Upper``
  pair per dimension, with ``Lower =< Upper``. Initial positions must be
  within bounds.
- ``fitness(+Position, -Fitness)`` — computes a numeric fitness value.
  The optimizer minimizes this value by default; use
  ``objective(maximize)`` to maximize it instead.

Optionally, a problem object may define:

- ``initial_velocities(-Velocities)`` — returns exactly one numeric
  velocity vector per initial position, with the same dimensions. Every
  component must be between plus or minus the corresponding position
  range, inclusively. When this predicate is not defined or fails,
  velocities are initialized randomly.
- ``stop_condition(+Iteration, +BestPosition, +BestFitness)`` — succeeds
  when the search should terminate early.
- ``progress(+Iteration, +BestPosition, +BestFitness, +MeanFitness, +Diversity)``
  — called periodically during optimization. Diversity is the mean
  Euclidean distance of particle positions from the swarm centroid. A
  final report is produced when progress reporting is enabled.

Options
-------

Options for the ``run/3-4`` predicates:

- ``objective(Objective)`` — optimization direction, either ``minimize``
  or ``maximize`` (default: ``minimize``).
- ``target_fitness(Fitness)`` — numeric target that stops the run when
  the best fitness reaches or passes it in the selected objective
  direction. Use ``none`` to disable target stopping (default:
  ``none``).
- ``max_iterations(N)`` — maximum number of swarm iterations (default:
  ``1000``).
- ``stagnation_iterations(N)`` — number of consecutive iterations
  without a strict global-best improvement before stopping. Set to ``0``
  to disable stagnation stopping (default: ``0``).
- ``inertia_weight(W)`` — non-negative float velocity inertia weight
  (default: ``0.7298``).
- ``cognitive_coefficient(C)`` — non-negative float personal-best
  acceleration coefficient (default: ``1.49618``).
- ``social_coefficient(C)`` — non-negative float global-best
  acceleration coefficient (default: ``1.49618``).
- ``updates(N)`` — number of progress reports during the run. Set to
  ``0`` to disable progress reporting (default: ``0``).
- ``seed(S)`` — positive integer random seed for reproducible runs.

Run statistics
--------------

The ``run/4`` predicate returns:

- ``iterations(N)`` — number of completed swarm iterations.
- ``evaluations(E)`` — number of fitness evaluations, including initial
  positions.
- ``improvements(I)`` — number of iterations that improved the global
  best.
- ``final_mean_fitness(M)`` — mean fitness of the final swarm positions.
- ``final_diversity(D)`` — diversity of the final swarm positions.

Fitness values are reported unchanged. Thus, ``BestFitness``,
``final_mean_fitness(M)``, and the fitness values passed to
``stop_condition/3`` and ``progress/5`` are always the values returned
by the problem ``fitness/2`` predicate. The ``objective/1`` option only
controls which values are considered better and the meaning of an
improvement.

Limitations
-----------

- Continuous numeric vectors only.
- Synchronous global-best topology only.
- No restarts, discrete particles, or custom topology.
- Problems supply positions, bounds, and optionally initial velocities.
  The optimizer owns subsequent velocity updates and cognitive/social
  randomness.

Usage
-----

.. _defining-a-problem-1:

Defining a problem
~~~~~~~~~~~~~~~~~~

::

   :- object(sphere,
       implements(particle_swarm_optimization_protocol)).

       initial_positions([
           [-4.0, -4.0],
           [ 4.0,  4.0],
           [ 0.5, -0.5],
           [ 3.0, -3.0]
       ]).

       position_bounds([(-5.0)-5.0, (-5.0)-5.0]).

       initial_velocities([
           [ 0.0,  0.0],
           [ 0.0,  0.0],
           [-0.1,  0.1],
           [-0.2,  0.2]
       ]).

       fitness([X, Y], Fitness) :-
           Fitness is X*X + Y*Y.

   :- end_object.

Running the algorithm
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- particle_swarm_optimization(sphere)::run(Position, Fitness).
   Position = [..., ...], Fitness = ...

Reproducible runs with statistics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- particle_swarm_optimization(sphere)::run(
            Position, Fitness, Statistics,
            [seed(42), max_iterations(500)]
        ).

Maximization
~~~~~~~~~~~~

::

   | ?- particle_swarm_optimization(Problem)::run(
            Position, Fitness,
            [objective(maximize), seed(42)]
        ).

Stopping after stagnation
~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- particle_swarm_optimization(Problem)::run(
            Position, Fitness, Statistics,
            [stagnation_iterations(25), seed(42)]
        ).

Stopping at a target fitness
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- particle_swarm_optimization(Problem)::run(
            Position, Fitness, Statistics,
            [target_fitness(0.001), seed(42)]
        ).

Using a custom random number generator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- particle_swarm_optimization(sphere, well512a)::run(
            Position, Fitness, [seed(42)]
        ).
