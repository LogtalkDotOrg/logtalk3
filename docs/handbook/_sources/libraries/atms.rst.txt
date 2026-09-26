.. _library_atms:

``atms``
========

This library implements the Horn fragment of Johan de Kleer's
Assumption-based Truth Maintenance System (ATMS). A system maintains
assumption nodes, ordinary nodes, Horn justifications, minimal labels,
and minimal nogoods. A label is an antichain of the minimal consistent
environments supporting a node.

API documentation
-----------------

Open the
`../../apis/library_index.html#atms <../../apis/library_index.html#atms>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(atms(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(atms(tester)).

API and state
-------------

The ``atms`` object uses functional state:

::

   | ?- atms::new(atms_bitset_environment, S0),
        atms::create_assumption(sensor_ok, S0, A, S1),
        atms::create_node(alarm, S1, Alarm, S2),
        atms::justify(Alarm, [A], sensor_rule, S2, S3),
        atms::label(Alarm, S3, Label).

``Label`` is ``[[node(0)]]``. Node identifiers are intentionally opaque;
use ``atms::nodes/2`` to obtain their associated data. State threading
is a deliberate portability choice: it supports multiple independent
ATMSs without dynamic predicates, global counters, or a dependence on a
particular Prolog backend.

``justify/5`` incrementally propagates only newly accepted label
environments via a reverse index from antecedent nodes to
justifications. A contradiction node is created with
``create_contradiction/4``; every environment in its label becomes a
nogood and prunes supersets from all labels immediately.

``interpretations/2`` enumerates maximal consistent environments. With
no nogoods, it returns the environment containing all assumptions
directly. When nogoods require subset search, it is intended for small
sets of assumptions.

Environment representations
---------------------------

The public API always accepts and returns environments as ordered lists
of node identifiers. The internal representation is selected with
``new/2`` and conforms to ``atms_environment_protocol``:

- ``atms_ordered_list_environment`` is the portable default. Storage is
  linear in the number of assumptions and union and subset use linear
  merges. It is a good choice for small or very sparse environments and
  is independent of integer bit width, but stores a full node term per
  assumption.
- ``atms_bitset_environment`` stores environments as non-negative
  integer bitsets. Union and subset each use one integer operation whose
  runtime cost scales with the represented bit width. It is compact and
  fast for dense environments with modest highest identifiers. Large
  gaps or high identifiers create wide integers, and the usable width
  can be backend-dependent.
- ``atms_segmented_bitset_environment`` stores canonical sparse ordered
  lists of nonempty 16-bit blocks. Storage is linear in the number of
  occupied blocks; union and subset use linear block merges and
  bounded-width bit operations. It is a good choice for larger
  environments or sparse identifier ranges with local clustering, but
  pair and list overhead can exceed ordered lists when assumptions
  occupy separate blocks.

Applications can provide another representation object implementing
``empty/1``, ``singleton/2``, ``union/3``, ``subset/2``, ``equal/2``,
``from_list/2``, and ``to_list/2``. This keeps the ATMS algorithm
independent of storage choices such as ordered sets, bit vectors, or a
future BDD implementation.

Limitations
-----------

- The library implements an add-only Horn ATMS. It does not provide
  non-Horn justifications, focus management, node or justification
  retraction, demand-driven labels, or recursive proof explanations. The
  ``why/3`` predicate reports only the direct stored justifications for
  a node, in insertion order.
- Node identifiers are scoped to a state lineage, not globally unique.
  Independently created states and sibling states can contain equal
  identifiers for different nodes. Applications must not pass a node
  identifier to an unrelated state, where it could silently denote a
  different local node.
- Labels and nogoods can contain exponentially many minimal environments
  in the number of assumptions. Combining antecedent labels can
  therefore require an exponential number of union and subset checks.
  Products are accumulated directly into the minimal antichain and
  inconsistent partial products are pruned, avoiding materialization of
  the complete Cartesian product.
- The functional state uses persistent AVL dictionaries for keyed
  indexes. Adding a nogood must nevertheless map over every node label,
  while label and nogood antichain operations require linear subset
  scans. These costs can dominate updates in systems with many nodes or
  large antichains.
- The number of maximal environments returned by ``interpretations/2``
  and the time required to find them can still be exponential in the
  number of assumptions. When nogoods are present, the implementation
  explores subsets incrementally and prunes inconsistent branches, so it
  does not materialize the complete powerset. With no nogoods, it
  returns the complete assumption environment directly.
- The bitset representation uses the global node identifier as the bit
  index. Its arithmetic and storage costs therefore depend on the
  highest node identifier, not just on the number of assumptions. Large
  or sparse node identifiers can be expensive, and the maximum usable
  identifier is backend-dependent where integer arithmetic is bounded.
  For example, on GNU Prolog, setting a bit for a sufficiently high node
  identifier can raise an ``evaluation_error(int_overflow)`` error. Use
  the ordered-list or segmented bitset representation when this limit
  may be reached.
- Alternative environment representations are trusted to implement
  canonical conversion, union, subset, and equality consistently.
  Protocol conformance is checked when creating a state, but these
  algebraic properties are not validated at runtime.

References
----------

- de Kleer, Johan. "An Assumption-Based TMS.” *Artificial Intelligence*
  28(2), 127–162, 1986. https://doi.org/10.1016/0004-3702(86)90080-9
- de Kleer, Johan. "Extending the ATMS.” *Artificial Intelligence*
  28(2), 163–196, 1986.
- de Kleer, Johan. "Problem Solving with the ATMS.” *Artificial
  Intelligence* 28(2), 197–224, 1986.
- de Kleer, Johan. "A General Labeling Algorithm for Assumption-Based
  Truth Maintenance.” *Proceedings of the Seventh National Conference on
  Artificial Intelligence (AAAI-88)*, 1988.
  https://cdn.aaai.org/AAAI/1988/AAAI88-034.pdf
- Forbus, Kenneth D., and Johan de Kleer. *Building Problem Solvers.*
  MIT Press, 1993.
