.. _library_simple_temporal_networks:

``simple_temporal_networks``
============================

This library provides the ``stn_protocol`` protocol and the ``stn``
object for Simple Temporal Networks (STNs). It is a metric substrate for
temporal reasoning, e.g., Temporal ATMS libraries. It reuses the
``intervals`` library for concrete Allen relation views.

States are opaque ground terms. Updates return a new state without
changing the original, allowing independent networks and branching
histories. All successfully returned states are consistent. Time-point
labels are distinct ground terms, compared by term identity; for
example, ``1`` and ``1.0`` name different time-points. The reference
label ``zero`` is reserved and denotes time zero. It is added
automatically and must not be supplied to ``new/2``.

Metric model
------------

An input ``constraint(X, Y, Delta)`` denotes:

::

   time(Y) - time(X) =< Delta

``Delta`` must be a finite integer or float. A directed edge from X to Y
therefore supplies an upper difference bound, not an earliest time.
Shortest-path distance ``d(X,Y)`` is the tightest upper bound on Y - X.
For consistent networks:

::

   difference_bounds(X,Y) = [-d(Y,X), d(X,Y)]
   earliest(T) = -d(T,zero)
   latest(T)   =  d(zero,T)

There is no implicit assumption that non-reference time-points are
non-negative. Missing directed paths yield the symbolic bound
``positive_infinity``; negating that bound yields ``negative_infinity``.
These atoms are not arithmetic values and must not be passed to
``is/2``. Self-distances and both bounds of ``zero`` are zero. A
consistent network may have one-sided or fully unbounded windows.

Bellman-Ford with initially zero potentials for every node detects
negative cycles, including components disconnected from ``zero``. A
consistent graph is then closed using Floyd-Warshall with fresh matrices
for successive intermediate nodes. No destructive term updates are used.

API overview
------------

Public modes, argument names, failure conditions, and explanation
formats are documented in ``stn_protocol.lgt`` using ``mode/2`` and
``info/2`` directives.

+----------------------------------+----------------------------------+
| Predicates                       | Purpose                          |
+==================================+==================================+
| ``new/2``, ``add_time_points/3`` | Construct a network or append    |
|                                  | fresh time-points                |
+----------------------------------+----------------------------------+
| ``remove_time_points/3``         | Remove time-points and all       |
|                                  | incident source constraints,     |
|                                  | rebuilding once                  |
+----------------------------------+----------------------------------+
| ``time_points/2``,               | Inspect a library-created state  |
| ``constraints/2``,               |                                  |
| ``consistent/1``                 |                                  |
+----------------------------------+----------------------------------+
| ``schedule/2``                   | Return one finite feasible time  |
|                                  | assignment                       |
+----------------------------------+----------------------------------+
| ``earliest_schedule/2``          | Return the componentwise         |
|                                  | earliest assignment when all     |
|                                  | lower bounds are finite          |
+----------------------------------+----------------------------------+
| ``add_constraint/5-6``           | Add one upper difference bound,  |
|                                  | optionally returning its ID      |
+----------------------------------+----------------------------------+
| ``add_constraints/3-4``          | Atomically add a batch,          |
|                                  | optionally returning its IDs     |
+----------------------------------+----------------------------------+
| ``try_add_constraints/4``        | Return an updated state or an    |
|                                  | identified negative-cycle        |
|                                  | witness                          |
+----------------------------------+----------------------------------+
| ``remove_constraints/3``         | Remove source constraints by ID  |
|                                  | and rebuild once                 |
+----------------------------------+----------------------------------+
| ``distance/4-5``                 | Query an upper difference bound, |
|                                  | optionally with one supporting   |
|                                  | path                             |
+----------------------------------+----------------------------------+
| ``earliest/3``, ``latest/3``,    | Query absolute time bounds       |
| ``bounds/4``                     |                                  |
+----------------------------------+----------------------------------+
| ``difference_bounds/5``          | Query both relative difference   |
|                                  | bounds                           |
+----------------------------------+----------------------------------+
| ``entails/4-5``                  | Test an upper difference bound,  |
|                                  | optionally with a supporting     |
|                                  | path                             |
+----------------------------------+----------------------------------+
| ``can_precede/3``,               | Test possible or entailed strict |
| ``must_precede/3``               | precedence                       |
+----------------------------------+----------------------------------+
| ``can_precede_or_equal/3``,      | Test possible or entailed        |
| ``must_precede_or_equal/3``      | non-strict precedence            |
+----------------------------------+----------------------------------+
| ``window_interval/3``            | Export a finite non-degenerate   |
|                                  | marginal window as numeric       |
|                                  | ``i(Start,End)``                 |
+----------------------------------+----------------------------------+
| ``event_interval/4``             | Export a fixed positive-duration |
|                                  | event as numeric                 |
|                                  | ``i(Start,End)``                 |
+----------------------------------+----------------------------------+
| ``window_relation/4``            | Classify finite non-degenerate   |
|                                  | marginal windows                 |
+----------------------------------+----------------------------------+
| ``event_relation/6``             | Classify two events whose four   |
|                                  | endpoints are fixed              |
+----------------------------------+----------------------------------+

Malformed public arguments, unknown time-point labels, and invalid
weights fail without binding input variables. State arguments must be
states returned by this API. Arithmetic evaluation and representation
errors propagate rather than being interpreted as temporal
contradictions. Empty update batches and empty time-point extensions
return the original state. Retraction accepts a list of positive integer
IDs, ignoring missing IDs and duplicate requests.

``remove_time_points/3`` accepts a ground list of labels and removes
those points together with every source constraint whose endpoint is
removed. Missing labels and duplicate requests are ignored; a request
containing ``zero`` fails atomically. Requests removing no existing
point return the original state, while isolated points are removed even
when no source changes. Surviving node/source order, source IDs, and the
next source ID are preserved. Queries for removed points fail because
those labels are no longer declared.

Schedules
---------

``schedule(STN, Schedule)`` returns one finite assignment as
``time(Point, Value)`` terms in declaration order, including
``time(zero, 0)`` first. It also assigns finite values to isolated
points, disconnected components, and points with unbounded absolute
windows. The original state and source identifiers are unchanged.
Repeated calls on the same state select the same assignment.

The assignment uses each cached closure column's minimum finite
distance, then shifts all values so the reference point is zero. In
exact arithmetic, these minima are shortest-path potentials from an
implicit source with zero-weight edges to every point. Normalization
preserves their differences. Every retained source inequality is checked
after normalization using backend arithmetic. A violation raises
``evaluation_error(stn_numerical_inconsistency)``; non-finite arithmetic
raises ``evaluation_error(float_overflow)``. No tolerance or numerical
repair is applied, and floating-point results remain approximate.

This selects one assignment, not an enumeration, optimization, execution
policy, or necessarily a non-negative or globally earliest schedule. An
unconstrained point need not receive zero when another component anchors
the reference normalization. See the schedule and numeric limitations
below.

``earliest_schedule(STN, Schedule)`` instead returns each point's finite
earliest absolute bound in the same format and declaration order, with
``time(zero, 0)`` first. Every declared point must have a finite lower
bound; finite upper bounds are not required. An unbounded lower bound
causes failure without binding the output. No artificial floor is added,
and the state and source identifiers are unchanged.

This reads the reference column of the cached closure and negates each
distance to ``zero``. In exact arithmetic, the resulting assignment
jointly attains every coordinate's minimum, including bounds propagated
through correlated points. It uses the same retained-source validation
as ``schedule/2``; floating-point feasibility and minimality are not
exact-real guarantees.

For example, propagate a lower bound through a duration constraint while
choosing the earliest value of an independently anchored point:

::

   | ?- stn::new([a,b,c], STN0),
        stn::add_constraints(STN0, [
          constraint(a,zero,-5), constraint(b,a,-2),
          constraint(c,zero,-1), constraint(zero,c,3)
        ], STN),
        stn::earliest_schedule(STN, Schedule).
   Schedule = [time(zero,0),time(a,5),time(b,7),time(c,1)].

Sources and explanations
------------------------

Every accepted source receives a monotonically allocated positive
integer ID. ``constraints/2`` returns ``constraint(Id,X,Y,Delta)`` terms
in insertion order, including parallel, redundant, and equal-weight
constraints. Synthetic zero diagonals are not source constraints.
Removing a stronger source can therefore restore a surviving weaker
bound or a tied alternative support. Removed IDs are not reused in
accepted descendant states.

Time-point deletion rebuilds from surviving source constraints, not from
cached implied distances. Bounds supported through a removed point can
therefore become weaker or unbounded. This is source retraction, not
variable elimination preserving the original network's consequences on
surviving points. Re-adding a deleted label with ``add_time_points/3``
creates an unconstrained point; its former incident sources are not
restored.

``try_add_constraints(STN0, Constraints, Ids, Outcome)`` returns either:

::

   consistent(STN)
   inconsistent(cycle(Sources, TotalWeight))

``Ids`` corresponds to the candidate batch in input order, even when
rejected. The cycle's identified source constraints form an ordered
closed directed path with negative total weight. It can contain both
existing sources and candidate sources. The original state is unchanged
in either case. The consistent-only insertion predicates fail for the
inconsistent outcome.

``distance(STN,X,Y,Upper,Path)`` returns one ordered source-constraint
path supporting a finite distance. It fails when the pair is
unreachable, while ``distance/4`` returns ``positive_infinity``.
Identity queries have distance zero and an empty path. With integer
weights, the path's weights sum exactly to the distance. Equal-weight
ties retain the earlier selected witness.

A Temporal ATMS can map source IDs to assumption supports and use a
cycle's supports to justify a contradiction. Mutually exclusive
assumption environments must be instantiated separately, or handled by a
future explicitly labelled distance algorithm. Retaining all sources is
essential for replay and retraction; returning one path is not an
enumeration of all ATMS labels.

Precedence and interval views
-----------------------------

For feasible Y - X bounds [Lower,Upper], ``can_precede(STN,X,Y)``
succeeds when Upper is positive or unbounded: some schedule has X < Y.
``must_precede/3`` requires a positive Lower: every schedule has X < Y.
The non-strict variants use non-negative bounds. Strict self-precedence
fails, while non-strict self-precedence succeeds.

``window_relation/4`` classifies two marginal uncertainty envelopes, not
two events. For example, two time-points constrained to be simultaneous
can both have window [0,2]; their envelope relation is ``equal``, but
their instants cannot strictly precede each other. Unconstrained points
with those same windows could occur in either order. Marginal envelopes
omit correlations.

``window_interval(STN,Point,Interval)`` exports one such envelope as
``i(Start,End)``, with finite numeric bounds and ``Start < End``. It
fails for unbounded or singleton windows, including the singleton
reference window. It exports an uncertainty envelope, not an event
duration.

``event_interval(STN,StartPoint,EndPoint,Interval)`` exports a concrete
event as ``i(Start,End)``. Both endpoints must have finite singleton
absolute bounds and the duration must be strictly positive. Unknown
points, invalid states, uncertain or unbounded endpoints, and reversed
or zero-duration events fail. Both exports preserve computed endpoint
numeric representations and leave the state and source identifiers
unchanged.

``event_relation(STN,Start1,End1,Start2,End2,Relation)`` instead
requires all four endpoints to have finite singleton absolute bounds,
and each event to have positive duration. Both views jointly rank their
four endpoints by arithmetic value and delegate to ``interval::new/3``
and ``interval::relation/3``. Shared ranks preserve arithmetic equality,
including equality between an integer and a float, without converting
integers to floats. This avoids using the interval object's term
ordering as numeric ordering. Ranked interval terms are internal to the
query and are not exported coordinates.

The exported ``i/2`` descriptors contain actual numeric coordinates, not
internal ranks. They use arithmetic endpoint ordering and are plain
numeric data, not a portable replacement for the ranked ``interval``
bridge. In particular, do not assume they can be passed directly to
term-order-based ``interval`` predicates across backends and mixed
numeric representations. Use ``window_relation/4`` or
``event_relation/6`` for portable Allen comparisons.

Relationship to interval constraint networks
--------------------------------------------

The ``intervals`` library already provides
``interval_constraint_network`` for qualitative temporal reasoning. Its
network infrastructure overlaps with this library, but the two solvers
address different constraint languages:

+----------------+--------------------------+----------------------------+
| Aspect         | Interval constraint      | STN                        |
|                | network                  |                            |
+================+==========================+============================+
| Nodes          | Symbolic intervals       | Numeric time-points        |
+----------------+--------------------------+----------------------------+
| Constraints    | Allen relation sets,     | Difference bounds, such as |
|                | such as                  | ``time(Y) - time(X) =< 5`` |
|                | ``[before, meets]``      |                            |
+----------------+--------------------------+----------------------------+
| Propagation    | Relation composition and | Numeric addition and       |
|                | intersection             | minimum                    |
+----------------+--------------------------+----------------------------+
| Contradictions | Empty relation sets      | Negative-weight cycles     |
+----------------+--------------------------+----------------------------+
| Results        | Qualitative relations    | Numeric bounds, schedules, |
|                | and explanations         | source-based witnesses     |
+----------------+--------------------------+----------------------------+

Allen relation-set propagation composes the relations along a path and
intersects the result with the existing pair constraint. STN closure
instead adds numeric path weights and selects tighter upper bounds.
Delegating STN closure to the qualitative solver would lose metric
information: an Allen relation alone cannot express a specific duration,
deadline, or numeric gap.

The consistency guarantees also differ. Path consistency is not a
complete satisfiability test for arbitrary disjunctive Allen networks.
In particular, ``interval_constraint_network::consistent/1`` checks that
explicit pair relation sets are non-empty; propagation must be requested
separately. For an ordinary STN, absence of negative cycles establishes
feasibility in exact arithmetic. This library performs that check when
constructing states; ``stn::consistent/1`` recognizes library-produced
states rather than recomputing their consistency. Floating-point
limitations still apply.

Node management, dense indexed storage, immutable updates, and
explanation bookkeeping have similar implementation patterns. These are
candidates for shared utilities, but do not make the two propagation
operations interchangeable or by themselves justify a shared solver
abstraction. Concrete Allen classification already delegates to
``interval``, rather than duplicating its relation algebra.

Future symbolic uncertain-event relation networks should build on
``interval_constraint_network``, reusing its refinement, propagation,
and explanation APIs instead of implementing another Allen
constraint-network solver inside ``stn``. The absence of an uncertain
Allen solver noted below is a limitation of this STN library, not of the
repository as a whole.

Combining qualitative interval constraints with metric endpoint
constraints would still require an explicit bridge. Independently
propagating the two networks does not establish their joint consistency.
The intended boundary is therefore to keep STN as the metric solver,
retain its interval predicates as adapters, and investigate shared
indexing or storage utilities separately.

Loading
-------

::

   | ?- logtalk_load(simple_temporal_networks(loader)).

The loader loads ``basic_types(loader)`` and ``intervals(loader)``,
followed by the protocol and implementation with optimizations enabled.

Testing
-------

::

   | ?- logtalk_load(simple_temporal_networks(tester)).

Tests include determinism checks, all 13 concrete Allen relations, an
exhaustive bounded integer-schedule oracle, and generated properties
using QuickCheck.

Examples
--------

Anchor a point to a finite window:

::

   | ?- stn::new([t], STN0),
        stn::add_constraints(STN0, [constraint(zero,t,10), constraint(t,zero,-5)], STN),
        stn::bounds(STN, t, Earliest, Latest).
   Earliest = 5, Latest = 10.

Export a mixed-representation numeric window:

::

   | ?- stn::new([t], STN0),
        stn::add_constraints(STN0, [constraint(zero,t,2), constraint(t,zero,-1.0)], STN),
        stn::window_interval(STN, t, Interval).
   Interval = i(1.0,2).

Export a fixed event's actual numeric coordinates:

::

   | ?- stn::new([start,end], STN0),
        stn::add_constraints(STN0, [
          constraint(zero,start,1.0), constraint(start,zero,-1.0),
          constraint(zero,end,2), constraint(end,zero,-2)
        ], STN),
        stn::event_interval(STN, start, end, Interval).
   Interval = i(1.0,2).

Constrain A's duration, anchor its start, and require A to end no later
than B starts. The precedence edge runs from B's start to A's end:

::

   | ?- stn::new([start_a,end_a,start_b,end_b], STN0),
        stn::add_constraints(STN0, [
            constraint(start_a,end_a,10), constraint(end_a,start_a,-5),
            constraint(zero,start_a,2), constraint(start_a,zero,0),
            constraint(start_b,end_a,0), constraint(zero,start_b,20)
        ], STN),
        stn::bounds(STN, start_a, Earliest, Latest),
        stn::must_precede_or_equal(STN, end_a, start_b),
        stn::window_relation(STN, start_a, start_b, Relation).
   Earliest = 0, Latest = 2, Relation = before.

Here ``before`` describes the start-point envelopes. B's end remains
unbounded, and ``event_relation/6`` would fail because the event
endpoints are not fixed. Fixing two events to [0,2] and [2,4] permits an
actual ``meets`` classification:

::

   | ?- stn::new([sa,ea,sb,eb], STN0),
        stn::add_constraints(STN0, [
            constraint(zero,sa,0), constraint(sa,zero,0),
            constraint(zero,ea,2), constraint(ea,zero,-2),
            constraint(zero,sb,2), constraint(sb,zero,-2),
            constraint(zero,eb,4), constraint(eb,zero,-4)
        ], STN),
        stn::event_relation(STN, sa, ea, sb, eb, Relation).
   Relation = meets.

Inspect a conflict without losing its source IDs:

::

   | ?- stn::new([a,b], STN0),
        stn::try_add_constraints(STN0,
            [constraint(a,b,1), constraint(b,a,-2)], Ids, Outcome).
   Ids = [1,2],
   Outcome = inconsistent(cycle(Sources, -1)).

``Sources`` contains ``constraint(1,a,b,1)`` and
``constraint(2,b,a,-2)`` in closed-path order. Cycle starting points may
depend on the network's declared node and source order. They do not
affect the conflict's meaning.

Remove an intermediate point and retract its incident sources. The
surviving direct constraint is weaker than the original path through the
removed point:

::

   | ?- stn::new([a,b,c], STN0),
      stn::add_constraints(STN0, [
        constraint(a,b,3), constraint(b,c,2), constraint(a,c,10)
      ], STN1),
      stn::distance(STN1, a, c, Before),
      stn::remove_time_points(STN1, [b], STN),
      stn::time_points(STN, TimePoints),
      stn::distance(STN, a, c, After).
   Before = 5, TimePoints = [zero,a,c], After = 10.

Generate a schedule with a positive anchor and a disconnected negative
edge:

::

   | ?- stn::new([anchor,a,b], STN0),
      stn::add_constraints(STN0, [
        constraint(anchor,zero,-5), constraint(zero,anchor,10),
        constraint(a,b,-2)
      ], STN),
      stn::schedule(STN, Schedule).
   Schedule = [time(zero,0),time(anchor,5),time(a,5),time(b,3)].

Limitations
-----------

- **Numeric domain and precision.** Only finite integers and floats are
  accepted as weights. Symbolic infinities are output bounds, not
  admissible weights. Native numeric infinities and NaNs are rejected
  where supported by the backend; rational or other numeric types are
  not accepted. Integer computations are exact within backend arithmetic
  limits. Mixed numeric arithmetic and floats can lose precision through
  rounding, cancellation, underflow, and accumulation. This affects
  bounds, zero comparisons, negative-cycle detection, and witnesses.
  There is no implicit epsilon or exact-real guarantee for
  floating-point results. Prefer suitably scaled integers when conflicts
  must soundly justify ATMS nogoods.

- **Numerical diagnostics.** Arithmetic evaluation, overflow, and
  backend representation errors propagate; they are not temporal
  contradictions. A non-finite computed sum raises
  ``evaluation_error(float_overflow)``. Contradictory numerical
  diagnostics, such as a detected cycle whose reconstructed weight is
  not negative, a negative closure diagonal after the consistency check,
  a cyclic witness reconstruction, or a generated schedule violating a
  source inequality after normalization, raise
  ``evaluation_error(stn_numerical_inconsistency)``. Float path/cycle
  sums can differ with evaluation order. No automatic rescaling,
  tolerance, numerical repair, or complete detection of floating-point
  inaccuracies is provided.

- **Expressiveness.** Stored constraints are closed, non-strict
  difference inequalities over real-valued time. No strict stored
  inequalities, disjunctive temporal constraints, contingent durations
  (STNUs), resource constraints, or optimization are provided. Strict
  precedence queries do not insert strict constraints. A cutoff policy
  and its current-time assumptions must be supplied by the caller.

- **Reference and unboundedness.** ``zero`` is reserved, fixed at zero,
  and cannot be supplied to ``new/2``, removed, or replaced by the
  caller. Points are not implicitly non-negative. Absolute bounds can be
  one-sided or completely unbounded; callers must handle symbolic
  infinities explicitly. Time-point labels must be declared, distinct,
  and ground. Numeric label identity differs from numeric value
  equality. Cyclic terms are not supported inputs.

- **Qualitative views.** ``window_relation/4`` requires finite
  non-degenerate windows and describes only marginal envelopes. It
  cannot establish possible or entailed Allen relations between
  uncertain events and omits joint endpoint correlations.
  ``event_relation/6`` requires four finite fixed endpoints and positive
  durations. Both views fail for zero-duration intervals and unbounded
  endpoints. There is no event registry or uncertain Allen relation
  solver.

- **Numeric interval exports.** ``window_interval/3`` and
  ``event_interval/4`` export only finite, non-degenerate numeric
  ``i/2`` descriptors, not internal ranks or certified term-ordered
  ``interval`` values. Mixed integer/float coordinates must not be
  assumed compatible with term-order-based interval operations on every
  backend. These exports do not add uncertain-event relation solving,
  event registration, zero-duration intervals, or support for unbounded
  descriptors. Numeric precision limitations still apply.

- **Schedule selection.** ``schedule/2`` returns one deterministic
  assignment, not all schedules, an optimal assignment, or a unique or
  globally earliest solution. ``earliest_schedule/2`` provides a
  componentwise earliest assignment only when every point has a finite
  lower bound; it fails otherwise rather than introducing floors or
  ignoring unbounded points. Its minimality guarantee is for exact
  arithmetic, not approximate floating-point results. Neither predicate
  enumerates schedules or optimizes general objectives. Times are not
  implicitly non-negative, and finite choices for unbounded points do
  not make their windows bounded. Floating-point normalization can lose
  differences; a failed source check raises a numerical diagnostic
  rather than returning an invalid assignment. Passing the check uses
  backend arithmetic, not exact-real certification. There is no
  automatic retry with another assignment, rescaling, or execution
  policy.

- **Witness completeness.** Only one deterministic supporting path or
  negative cycle is returned. Witnesses need not be minimal conflicts or
  minimal supports, and alternatives, all cycles, all minimal
  inconsistent subsets, and all minimal nogoods are not enumerated. ATMS
  labels, assumption mapping, and conflict minimization belong to the
  caller. A single merged STN cannot represent mutually exclusive
  environments.

- **Identifiers and states.** Source IDs are unique only along an
  accepted state lineage, not across independent networks or forks.
  Rejected IDs can coincide with subsequent allocations from the
  unchanged input state. States are opaque; constructing or altering
  their internal terms, or assuming a stable serialization format, is
  unsupported. ``consistent/1`` recognizes library-produced states; it
  does not recompute or certify an arbitrary hand-built graph or
  closure.

- **Point deletion.** Removing a time-point also retracts all its
  incident source constraints. It does not perform variable elimination
  preserving implied bounds between surviving points, which can
  consequently weaken or become unbounded. Re-adding a removed label
  does not restore its former constraints. The reference point ``zero``
  cannot be deleted.

- **Performance and backend limits.** The closure is dense, using O(n^2)
  cached cells plus O(m) retained sources for n time-points (including
  zero) and m constraints. Floyd-Warshall takes O(n^3) cell operations.
  This first implementation uses linear node lookup and list-based
  Bellman-Ford labels, yielding O(n^2 m) worst-case consistency work.
  Updates, source retractions, and point additions or deletions rebuild
  once; there is no incremental O(n^2) update guarantee. A cached pair
  cell is accessed in constant time after indexing, but each public call
  also traverses the ground state and uses linear node lookup; query
  cost is not constant in network size. Witness extraction is linear in
  path length after validation. ``schedule/2`` scans O(n^2) cells, while
  ``earliest_schedule/2`` scans O(n) reference-column cells. Both check
  m sources using linear label lookup. Including full state traversal,
  each takes O(n^2 + nm) work and O(n) temporary storage in addition to
  the existing state. Multiple retained snapshots increase live memory
  use. Matrix dimensions are subject to backend compound-arity and
  memory limits. Intended for small-to-medium networks, not a
  large-scale or latency-sensitive solver.

- **Verification scope.** Bounded exhaustive fixtures and generated
  tests are not proofs of correctness or exhaustive checks over all
  networks or floating-point behavior.
