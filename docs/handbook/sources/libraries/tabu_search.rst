.. _library_tabu_search:

``tabu_search``
===============

Tabu search is a metaheuristic that guides a local-search procedure by
using a short-term memory structure (the *tabu list*) to avoid
revisiting recently explored solutions and to escape local minima. It is
particularly useful for combinatorial optimization problems such as the
Traveling Salesman Problem (TSP), graph coloring, and scheduling.

At each iteration, a set of candidate neighbors is examined and the best
admissible neighbor (non-tabu or allowed by aspiration) is selected.

The library provides the parametric object
``tabu_search(Problem, RandomAlgorithm)`` where ``Problem`` is an object
implementing the ``tabu_search_problem_protocol`` protocol and
``RandomAlgorithm`` is one of the algorithms supported by the
``fast_random`` library. The algorithm minimizes the energy (cost)
function defined by the problem.

A convenience object ``tabu_search(Problem)`` is also provided, using
the Xoshiro128++ random number generator (``xoshiro128pp``) as the
default.

API documentation
-----------------

Open the
`../../apis/library_index.html#tabu-search <../../apis/library_index.html#tabu-search>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(tabu_search(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(tabu_search(tester)).

Features
--------

- **Configurable random number generator** - the algorithm is
  parameterized by a ``fast_random`` algorithm. Available algorithms
  include ``xoshiro128pp``, ``xoshiro128ss``, ``xoshiro256pp``,
  ``xoshiro256ss``, ``well512a``, ``splitmix64``, and ``as183``. The
  convenience object ``tabu_search(Problem)`` defaults to
  ``xoshiro128pp``.
- **Tabu list (short-term memory)** - recently visited states or their
  ``tabu_key/2`` keys are stored with an expiration step. A state left
  by a move is forbidden during the next ``T`` candidate-selection
  steps. With fixed tenure (``tabu_tenure(T)``) the list behaves as a
  FIFO of maximum length ``T``. With ``tabu_tenure_range(Min, Max)``
  each accepted move receives a random tenure drawn uniformly from the
  inclusive range. A problem-defined ``tabu_tenure/4`` schedule takes
  precedence over both options. Candidates whose keys appear in the
  active list are forbidden unless the aspiration criterion is met.
- **Aspiration criterion** - a tabu candidate is accepted when its
  energy is strictly better than the best energy found so far (classic
  “best-so-far” aspiration), unless the problem supplies its own
  ``aspiration/3`` policy.
- **Candidate sampling** - by default the algorithm samples
  ``candidates(N)`` neighbors per iteration using ``neighbor_state/2``
  (or ``neighbor_state/3``). If the problem defines ``neighbors/2``,
  that complete list is used in its original order when it fits the
  candidate limit. Otherwise, a uniform subset is selected without
  replacement by position and shuffled using the configured random
  number generator.
- **Exhaustive evaluation** - ``exhaustive(true)`` evaluates the entire
  ``neighbors/2`` list in source order, regardless of ``candidates(N)``,
  without subset sampling. Enumeration must be implemented and succeed
  for every visited state; an empty list is valid.
- **Delta-energy optimization** - when the problem object defines
  ``neighbor_state/3``, the algorithm uses the returned energy delta
  directly instead of calling ``state_energy/2`` on the neighbor. This
  is useful when computing the energy change is cheaper than recomputing
  the full energy.
- **Best state tracking** - the algorithm tracks the best state found
  across all iterations and across all restart cycles, not just the
  final state.
- **Progress reporting** - if the problem object defines ``progress/5``,
  it is called periodically with completed global steps, best and
  current energies, and acceptance and improvement rates since the
  preceding report. The ``updates(N)`` option controls the reporting
  interval. When enabled, each cycle produces a final report instead of
  any coincident periodic report. A zero-step interval has zero rates.
- **Run statistics** - the ``run/4`` predicate returns a list of
  statistics including the number of steps, acceptances, improvements,
  and the final tabu-list size.
- **Seed control** - the ``seed(S)`` option initializes the random
  number generator for reproducible runs. A problem using an independent
  generator must also seed that generator; the option only controls the
  configured ``fast_random`` algorithm.
- **Restarts** - the ``restarts(N)`` option runs N additional
  tabu-search cycles after the first, with a cleared tabu list. The
  problem can supply a diversified state using ``restart_state/2``;
  otherwise each restart begins from the global best. Statistics
  accumulate across all cycles.

Defining a problem
------------------

A problem object implements the ``tabu_search_problem_protocol``
protocol and defines the following predicates, plus at least one
neighborhood predicate:

- ``initial_state(-State)`` - returns the starting state.
- ``state_energy(+State, -Energy)`` - computes the cost of a state (to
  be minimized).

Optionally, the problem object may also define:

- ``neighbor_state(+State, -Neighbor)`` - generates a neighboring state.
- ``neighbor_state(+State, -Neighbor, -DeltaEnergy)`` - generates a
  neighboring state and returns the energy change directly, avoiding a
  full energy recomputation.
- ``neighbors(+State, -Neighbors)`` - returns the complete list of
  neighboring states. When defined, the algorithm uses this list (or a
  random sample controlled by ``candidates(N)``) instead of repeated
  neighbor generation. The ``exhaustive(true)`` option evaluates the
  entire list.
- ``stop_condition(+Step, +BestEnergy, +CurrentEnergy)`` - succeeds when
  the search should terminate early.
- ``progress(+Step, +BestEnergy, +CurrentEnergy, +AcceptanceRate, +ImprovementRate)``

  - called periodically during the optimization to report progress.

- ``tabu_key(+State, -Key)`` - returns a compact or canonical
  tabu-equivalence key. Without this hook, the state itself is used.
  Keys are compared using ``==/2``, without instantiating either key.
  Implementations must return a nonvar key deterministically and must
  not instantiate state variables.
- ``restart_state(+BestState, -RestartState)`` - supplies a diversified
  restart state between cycles. Without this hook, the best state and
  its cached energy are reused. The hook must return a nonvar state
  deterministically without instantiating the best state. Its energy is
  recomputed once; a better restart immediately updates the global best,
  while ties preserve the incumbent. Restart construction does not
  increment move statistics.
- ``tabu_tenure(+Step, +BestEnergy, +CurrentEnergy, -Tenure)`` - returns
  a non-negative integer tenure deterministically for each accepted
  move, using the zero-based global selection step and pre-move
  energies. It overrides ranged and fixed options without consuming a
  ranged-tenure random draw. Zero skips insertion while preserving
  earlier active entries. Expiration steps already assigned to entries
  never change.
- ``aspiration(+CandidateState, +CandidateEnergy, +BestEnergy)`` -
  optionally permits an otherwise tabu contender. Success permits the
  move; failure rejects it even when it improves the global best.
  Non-tabu candidates are admissible without this call. The original
  state, not its key, is passed. Implementations must not instantiate
  the candidate and should be pure: candidates unable to win selection
  do not trigger the hook.

For example, a structured state can use only its identifier as a key:

::

   tabu_key(state(Identifier, _Payload), key(Identifier)).

Keys must remain stable throughout a run. Shared input variables retain
their identity; fresh variables do not produce stable equivalence across
calls. Energy evaluation and returned best solutions always use the
original states. An implemented hook that fails raises
``domain_error(tabu_hook_result, tabu_key/2)``; an unbound result raises
``domain_error(tabu_hook_result, tabu_key/2-Key)``. Exceptions thrown by
any problem hook propagate unchanged. Availability of the four policy
hooks is discovered once per run, including inherited and category
implementations; changing it during a run is not supported. These
input-preservation rules also apply to ``restart_state/2`` and
``aspiration/3``. Failure or an unbound restart raises the corresponding
``domain_error(tabu_hook_result, restart_state/2)`` or
``domain_error(tabu_hook_result, restart_state/2-RestartState)``. The
same contract applies to ``tabu_tenure/4``: failure reports its
indicator, and an invalid result reports ``tabu_tenure/4-Tenure`` in the
domain error.

Options
-------

Options for the ``run/3-4`` predicates:

- ``max_steps(N)`` - maximum number of iterations per cycle (default:
  ``10000``).
- ``tabu_tenure(T)`` - fixed tabu tenure: lifetime in steps of each tabu
  entry (default: ``7``). Ignored when ``tabu_tenure_range/2`` or a
  ``tabu_tenure/4`` implementation is present.
- ``tabu_tenure_range(Min, Max)`` - random tabu tenure: on each accepted
  move a tenure is drawn uniformly from the inclusive integer range
  ``Min..Max``. Overrides ``tabu_tenure/1`` when present, but is ignored
  when the problem implements ``tabu_tenure/4``.
- ``candidates(N)`` - number of candidate neighbors examined per
  iteration (default: ``20``).
- ``exhaustive(Boolean)`` - evaluate all enumerated neighbors in source
  order when ``true`` (default: ``false``). Requires an implemented
  ``neighbors/2``; its failure is an error. The candidate limit is
  ignored but still validated.
- ``updates(N)`` - number of progress reports during the run. Progress
  is reported by calling ``progress/5`` on the problem object. Set to
  ``0`` to disable (default: ``0``).
- ``seed(S)`` - positive integer seed for the random number generator,
  enabling reproducible runs (default: none).
- ``restarts(N)`` - number of additional tabu-search cycles after the
  first. Each restart clears the tabu list and uses ``restart_state/2``
  when defined, otherwise starting from the best state found so far
  (default: ``0``).

Run statistics
--------------

The ``run/4`` predicate returns a list of statistics about the completed
run:

- ``steps(N)`` - total number of steps executed.
- ``acceptances(A)`` - number of accepted moves.
- ``improvements(I)`` - number of moves that strictly improved the best
  energy found. Better restart states update the best but do not
  increment this move counter.
- ``final_tabu_size(S)`` - size of the tabu list at termination.

Performance
-----------

For an enumerated neighborhood of size ``M`` and candidate limit
``C < M``, sampling uses one list pass and shuffles only the selected
subset. When at most 20 positions need to be included or excluded,
uniform random positions are drawn first; other cases use sequential
selection with inclusion probability equal to the number still needed
divided by the number remaining. Expected sampling work is
``O(M + C log C)``, with ``O(C)`` auxiliary storage. When the full
neighborhood fits the limit, its order is preserved.

Candidates unable to improve an already selected candidate do not incur
tabu-membership scans. Candidates improving the global best satisfy
default aspiration without a scan. Custom aspiration first checks
membership and is called only for tabu contenders. Other candidates
compare against active keys, so large state terms and long tenures can
increase membership costs.

The optional ``tabu_key/2`` hook can reduce stored-term size and
membership comparison costs. It is called only for insertion or
membership checks that are needed; full states remain available for
energy evaluation and results. Exhaustive mode bypasses subset selection
but still materializes and evaluates the full list. Long custom tenures
may retain more entries than a later short tenure; entries expire
independently rather than by a fixed capacity.

Usage
-----

Example of a problem definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Define an object implementing the ``tabu_search_problem_protocol``
protocol. For example, a simple quadratic minimization problem:

::

   :- object(quadratic,
       implements(tabu_search_problem_protocol)).

       initial_state(50.0).
       neighbor_state(X, Y) :-
           fast_random(xoshiro128pp)::random(-5.0, 5.0, Delta),
           Y is X + Delta.
       state_energy(X, E) :-
           E is (X - 3.0) * (X - 3.0).

   :- end_object.

Running the algorithm
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- tabu_search(quadratic)::run(State, Energy).
   State = 3.00..., Energy = 0.000...

Running with custom options
~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- tabu_search(quadratic)::run(State, Energy, [max_steps(5000), tabu_tenure(10), candidates(30)]).
   State = 3.00..., Energy = 0.000...

Random tabu tenure
~~~~~~~~~~~~~~~~~~

::

   | ?- tabu_search(quadratic)::run(State, Energy, [tabu_tenure_range(5, 12), max_steps(5000)]).
   State = 3.00..., Energy = 0.000...

Exhaustive neighborhoods
~~~~~~~~~~~~~~~~~~~~~~~~

For a problem implementing ``neighbors/2``, evaluate all neighbors:

::

   | ?- tabu_search(Problem)::run(State, Energy, [exhaustive(true)]).

Missing enumeration raises
``existence_error(procedure, Problem::neighbors/2)`` before state
initialization. An implemented enumeration predicate that fails raises
``domain_error(tabu_hook_result, neighbors/2)``; its exceptions
propagate. The full list is materialized, so exhaustive mode can be
costly for large neighborhoods.

Problem-controlled tenure
~~~~~~~~~~~~~~~~~~~~~~~~~

A problem can vary tenure according to the global selection step:

::

   tabu_tenure(Step, _BestEnergy, _CurrentEnergy, Tenure) :-
       Tenure is 3 + Step mod 5.

Running with statistics
~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- tabu_search(quadratic)::run(State, Energy, Stats, []).
   State = 3.00..., Energy = 0.000...,
   Stats = [steps(10000), acceptances(...), improvements(...), final_tabu_size(...)]

Reproducible runs with seed
~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- tabu_search(quadratic)::run(S1, E1, [seed(42)]),
        tabu_search(quadratic)::run(S2, E2, [seed(42)]).
   S1 = S2, E1 = E2.

Restarts
~~~~~~~~

Run 3 tabu-search cycles (1 initial + 2 restarts). Each restart clears
the tabu list and begins from the best state found so far:

::

   | ?- tabu_search(quadratic)::run(State, Energy, [restarts(2)]).
   State = 3.00..., Energy = 0.000...

Restart diversification
~~~~~~~~~~~~~~~~~~~~~~~

A problem can diversify each restart, for example by perturbing a
numeric best state:

::

   restart_state(Best, State) :-
       State is Best + 5.

The original best remains available even when the perturbation worsens
energy. Stopping and progress reporting see the recomputed restart
energy before any move in the next cycle.

Custom aspiration
~~~~~~~~~~~~~~~~~

A problem can permit tabu moves whose energy does not exceed the global
best:

::

   aspiration(_CandidateState, CandidateEnergy, BestEnergy) :-
       CandidateEnergy =< BestEnergy.

This replaces the default strict-improvement criterion. Failure rejects
a tabu contender rather than falling back to that default.

Using a custom random number generator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the two-parameter version to select a specific ``fast_random``
algorithm:

::

   | ?- tabu_search(quadratic, well512a)::run(State, Energy).
   State = 3.00..., Energy = 0.000...

   | ?- tabu_search(quadratic, xoshiro256ss)::run(State, Energy, [seed(42)]).
   State = 3.00..., Energy = 0.000...

Limitations
-----------

- **Solution-based tabu memory only** - the tabu list stores states or
  their problem-defined ``tabu_key/2`` keys rather than move attributes.
  Compact keys reduce memory use and identify equivalent solutions, but
  solution-based memory can still be large for richly structured states
  and is often less effective than attribute-based tabu for problems
  where the relevant forbidden features are local (e.g. edges in TSP,
  variable assignments in scheduling).

- **No built-in reactive controller** - ``tabu_tenure/4`` supports
  problem-controlled adaptive schedules, but automatic cycle detection
  and reactive-tenure histories are not provided by the library.

- **No built-in aspiration strategy catalog** - classic best-so-far is
  the default rule. Other strategies can be supplied by
  ``aspiration/3``, but the library does not provide additional strategy
  implementations.

- **Neighborhood exploration** - when the problem does not define
  ``neighbors/2``, the algorithm samples a fixed number of candidates
  via repeated calls to ``neighbor_state/2`` or ``neighbor_state/3``.
  Exhaustive mode evaluates a problem-provided list, but streaming
  enumeration of large neighborhoods is not supported.

- **Short-term memory only** - there is no intermediate-term or
  long-term memory (frequency-based or elite-set structures) and
  therefore no built-in intensification. Diversification can be supplied
  by ``restart_state/2``; the default ``restarts(N)`` mechanism clears
  the tabu list and resumes from the best state found so far without
  built-in perturbations.

- **Key identity** - tabu membership is decided by strict term identity
  (``==/2``) of states or optional ``tabu_key/2`` keys, without
  instantiating either. Non-ground states are allowed; distinct
  variables are not identical. States that are semantically equivalent
  but not term-identical (e.g. rotations or reflections of a tour) are
  treated as distinct unless the problem normalizes them or supplies
  canonical keys.

- **Single trajectory** - the search follows one solution path at a
  time; population-based or multi-threaded variants are not currently
  implemented.

References
----------

The following references describe the tabu-search framework, including
short-term memory, tabu tenure, candidate selection, and aspiration
criteria used by this library. They also discuss more advanced
strategies beyond the implemented solution-based short-term memory
approach.

- Glover, F. (1989). "Tabu Search - Part I." *ORSA Journal on
  Computing*, 1(3), 190-206.
  `doi:10.1287/ijoc.1.3.190 <https://doi.org/10.1287/ijoc.1.3.190>`__.
- Glover, F. (1990). "Tabu Search - Part II." *ORSA Journal on
  Computing*, 2(1), 4-32.
  `doi:10.1287/ijoc.2.1.4 <https://doi.org/10.1287/ijoc.2.1.4>`__.
- Glover, F., and Laguna, M. (1997). *Tabu Search*. Springer US.
  `doi:10.1007/978-1-4615-6089-0 <https://doi.org/10.1007/978-1-4615-6089-0>`__.
