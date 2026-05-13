.. _library_intervals:

``intervals``
=============

This library provides:

- an ``interval_protocol`` protocol and an ``interval`` object
  implementing the 13 Allen base interval relations plus interval pair
  classification using ``relation/3``

- an ``interval_algebra_protocol`` protocol and an ``interval_algebra``
  object implementing Allen relation algebra predicates over the 13 base
  relation atoms

- an ``interval_relation_set_protocol`` protocol and an
  ``interval_relation_set`` object implementing canonical relation-set
  operations over Allen base relation atoms

- an ``interval_constraint_network_protocol`` protocol and an
  ``interval_constraint_network`` object implementing first interval
  constraint-network predicates over canonical relation sets, including
  batch refinement, explanation lists, and network inspection/comparison
  helpers

The ``interval`` object works on interval terms represented as
``i(Start, End)``. The ``interval_algebra`` object works on relation
atoms such as ``before``, ``overlaps``, and ``contains``. The
``interval_relation_set`` object works on canonical ordered
duplicate-free lists of relation atoms. The
``interval_constraint_network`` object works on
``network(Nodes, Constraints)`` terms whose pair constraints are
relation sets. Internally, the ``interval_constraint_network`` object
compiles that public representation to an a dense indexed form for
propagation and for most query operations, while still returning
ordinary ``network/2`` terms at the API boundary.

The ``interval_algebra::compose/3`` predicate returns canonical ordered
duplicate-free lists of base relation atoms.

Practical limits
----------------

The ``interval_constraint_network`` object is intended for
small-to-medium symbolic networks. It uses a complete
``network(Nodes, Constraints)`` representation, so the number of stored
pair constraints grows quadratically with the number of nodes.
Propagation is based on path consistency and now uses an internal dense
indexed representation with direct pair access, a reverse node-to-index
table, and scheduled-pair tracking to avoid repeated linear worklist
duplicate checks. This substantially reduces overhead relative to a
purely list-based implementation, but the representation is still dense
and updates still rebuild parts of the internal matrix before the result
is decompiled back to a public ``network/2`` term.

In practice, the network predicates are suitable for moderate
qualitative temporal reasoning workloads, but they should not be treated
as a large-scale or latency-sensitive temporal CSP solver. Exact limits
depend on the Prolog backend, network density, how often closure is
recomputed, and how often callers invoke single-step API operations that
must compile or decompile the public network representation.

API documentation
-----------------

Open the
`../../apis/library_index.html#intervals <../../apis/library_index.html#intervals>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(intervals(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(intervals(tester)).

Examples
--------

Classify two valid intervals according to their unique Allen base
relation:

::

   | ?- interval::new(1, 3, I1), interval::new(3, 5, I2), interval::relation(I1, I2, Relation).
   Relation = meets.

Compose two Allen base relation atoms:

::

   | ?- interval_algebra::compose(meets, met_by, Relations).
   Relations = [finishes, finished_by, equal].

Normalize an Allen relation set:

::

   | ?- interval_relation_set::normalize([during, before, during], RelationSet).
   RelationSet = [before, during].

Propagate interval constraints across a small symbolic network:

::

   | ?- interval_constraint_network::new([a, b, c], Network0),
        interval_constraint_network::refine(Network0, a, b, [before], Network1),
        interval_constraint_network::refine(Network1, b, c, [before], Network2),
        interval_constraint_network::path_consistency(Network2, Network3),
        interval_constraint_network::relation(Network3, a, c, RelationSet).
   RelationSet = [before].

Check whether a query is entailed by the current symbolic constraints:

::

   | ?- interval_constraint_network::entails(Network3, a, c, [before, meets]).
   true.

Inspect a simple explanation for an entailed relation:

::

   | ?- interval_constraint_network::entails(Network3, a, c, [before], Explanation).
   Explanation = propagated(b, [before], [before], [before]).

Inspect a simple contradiction explanation after closure:

::

   | ?- interval_constraint_network::contradiction(Closure, Explanation).
   Explanation = contradiction(a, c, propagated(b, [before], [before], [before])).

Refine and propagate in a single operation that fails on contradiction:

::

   | ?- interval_constraint_network::new([a, b, c], Network0),
        interval_constraint_network::refine_propagate(Network0, a, b, [before], Network1).
   Network1 = ...

Post a batch of constraints and propagate them in one step:

::

   | ?- interval_constraint_network::refine_propagate(Network0, [constraint(a, b, [before]), constraint(b, c, [before])], Network1).
   Network1 = ...

Collect direct and propagated changes while propagating:

::

   | ?- interval_constraint_network::propagate(Network2, Network3, Changes).
   Changes = [...].

Query which node triples caused the propagated refinements:

::

   | ?- interval_constraint_network::propagation_triples(Changes, Triples).
   Triples = [triple(a, b, c)].

List all immediate explanations supporting an entailed query:

::

   | ?- interval_constraint_network::entailment_explanations(Network3, a, c, [before, meets], Explanations).
   Explanations = [propagated(b, [before], [before], [before])].

List all contradiction explanations currently present in an inconsistent
network:

::

   | ?- interval_constraint_network::contradiction_explanations(Closure, Explanations).
   Explanations = [...].

Inspect a closure and compare networks by generality or equivalence:

::

   | ?- interval_constraint_network::constraints(Network3, Constraints).
   Constraints = [constraint(a, b, [before]), constraint(a, c, [before]), constraint(b, c, [before])].

   | ?- interval_constraint_network::subsumes(Network0, Network3).
   true.

   | ?- interval_constraint_network::equivalent(Network3, Network5).
   true.
