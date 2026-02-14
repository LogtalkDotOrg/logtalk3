.. _library_datalog:

``datalog``
===========

This library provides a portable Datalog and incremental rule engine for
educational purposes and non-demanding applications. This library is
work-in-progress and changes, possibly breaking backwards compatibility,
are to be expected.

API documentation
-----------------

Open the
`../../apis/library_index.html#datalog <../../apis/library_index.html#datalog>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(datalog(loader)).

Testing
-------

To run the library unit tests, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(datalog(tester)).

Scope
-----

- positive and stratified negation rules (``neg/1`` body literals)
- conservative aggregates (``agg(Op, Template, Goals, Result)``) with
  lower-strata dependencies (``Op`` in ``count``, ``sum``, ``min``,
  ``max``)
- safe rules (``Head`` and negated literal variables must appear in
  positive ``Body`` literals)
- fixpoint materialization
- incremental updates using support-count based propagation
- simple explanations for derived facts
- rule management (``add_rule/3``, ``remove_rule/1``)
- transaction support (``begin/0``, ``commit/0``, ``rollback/0``)
- stratum introspection (``predicate_stratum/3``, ``strata/1``)
- rule body normalization (positive ground literals first, then positive
  non-ground, then aggregates, then negation)

Public API overview
-------------------

- lifecycle: ``clear/0``, ``load_program/1``, ``materialize/0``
- rule management: ``add_rule/3``, ``remove_rule/1``, ``rules/1``
- fact management: ``assert_fact/1``, ``retract_fact/1``, ``facts/1``
- querying and explanations: ``query/1``, ``query/2``, ``explain/2``
- updates: ``update/3``
- transactions: ``begin/0``, ``commit/0``, ``rollback/0``
- stratification introspection: ``predicate_stratum/3``, ``strata/1``

Rules and facts are represented as terms:

- ``rule(Id, Head, Body)`` where ``Body`` is a list of ``Literal``
  terms:

  - positive literal: ``Term``
  - negative literal: ``neg(Term)``
  - aggregate literal: ``agg(Op, Template, Goals, Result)`` where ``Op``
    is one of ``count``, ``sum``, ``min``, ``max``

- ``fact(Fact)`` for extensional database (EDB) facts

Aggregate notes:

- Aggregate dependencies are required to be in lower strata.
- ``min`` and ``max`` require at least one matching value; otherwise no
  fact is derived.
- Incremental updates involving negation or aggregates currently
  fallback to full rematerialization for correctness.

Basic usage
-----------

::

   | ?- logtalk_load(datalog(loader)).
   ...

   | ?- Program = [
           rule(path_base, path(X,Y), [edge(X,Y)]),
           rule(path_rec, path(X,Z), [edge(X,Y), path(Y,Z)]),
           fact(edge(a,b)),
           fact(edge(b,c))
       ],
       datalog::load_program(Program),
       datalog::query(path(a,c)).

Limitations
-----------

This first version intentionally keeps the implementation simple and
portable. Negation support is currently limited to stratified programs.
Aggregates and cost-based optimization are planned future enhancements.
Aggregate support is currently limited to ``count``, ``sum``, ``min``,
and ``max``.
