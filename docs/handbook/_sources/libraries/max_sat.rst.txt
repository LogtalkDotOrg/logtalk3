.. _library_max_sat:

``max_sat``
===========

This library provides weighted partial MaxSAT solving by interfacing
with external solver command-line tools. It follows the design of the
``sat`` library: problems are written as ordered, ground statements with
term-valued variables (e.g., ``queen(Row, Column)``), serialized to WCNF
source, solved by an external process, and the solver output is decoded
into a collected result that maps solver variable numbers back to terms.

The supported solvers, selected using the ``solver(Name)`` option, are:

- ``open_wbo``: `Open-WBO <https://github.com/sat-group/open-wbo>`__
  (default executable: ``open-wbo``)
- ``rc2``: `RC2 <https://pysathq.github.io/>`__ from PySAT (default
  executable: ``rc2.py``)
- ``z3``: `Z3 <https://github.com/Z3Prover/z3>`__, the default solver
  (default executable: ``z3``)

Other solvers that follow the MaxSAT Evaluation conventions (an ``o``
line with the cost, an ``s`` status line, and a ``v`` line with the
model as literals) can be added by extending the ``max_sat_output`` and
``max_sat`` objects.

API documentation
-----------------

Open the
`../../apis/library_index.html#max_sat <../../apis/library_index.html#max_sat>`__
file in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(max_sat(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(max_sat(tester)).

Tests that require a solver are conditional on the solver executable
being available in the system path.

Problems
--------

A problem is a list of statements or an object implementing the
``max_sat_problem_protocol`` protocol, whose facts are compiled to
``wcnf_statement/2`` clauses by the ``max_sat_expansion`` hook (as with
the ``sat`` library, use the ``hook(max_sat_expansion)`` flag when
compiling the object). Statements are:

- hard clauses: a disjunction of literals (``+Term``, ``-Term``, or a
  bare ``Term``, equivalent to ``+Term``); the constant ``false`` is the
  empty clause
- soft clauses: ``soft(Weight, Clause)`` with a positive integer weight,
  or ``soft(Clause)`` for a weight of 1; the solver minimizes the sum of
  the weights of the falsified soft clauses
- ``comment(Text)`` and ``raw(Text)``

Disjunctions used as arguments must be parenthesized (e.g.,
``soft(2, (p ; -q))``) to be portable across backends. The functors
``soft/1-2``, ``comment/1``, and ``raw/1`` and the constant ``false``
cannot be used as bare propositional terms.

As an example, consider an object definition of the weighted minimum
vertex cover of a triangle:

::

   :- object(vertex_cover,
       implements(max_sat_problem_protocol)).

       in(a) ; in(b).
       in(b) ; in(c).
       in(a) ; in(c).
       soft(3, -in(a)).
       soft(1, -in(b)).
       soft(2, -in(c)).

   :- end_object.

Assuming the object saved in a ``vertex_cover.lgt`` file:

::

   | ?- logtalk_load(vertex_cover, hook(max_sat_expansion)).
   ...

We can then solve the problem and print the result:

::

   | ?- max_sat::solve(object(vertex_cover), Result),
        max_sat::print_result(Result).
   o 3
   s OPTIMUM FOUND
   v -1 2 3

The ``max_sat::cost/2`` and ``max_sat::literal/2`` predicates give
access to the cost and to the model literals using the problem terms:

::

   | ?- max_sat::solve(object(vertex_cover), Result),
        max_sat::cost(Result, Cost),
        findall(Literal, max_sat::literal(Result, Literal), Literals).
   Cost = 3,
   Literals = [-in(a), +in(b), +in(c)].

Sources
-------

The ``solve/2-3`` and ``program/1-3`` predicates accept the following
sources:

- ``object(Object)``: an object implementing
  ``max_sat_problem_protocol``
- ``terms(Statements)``: a list of statements
- ``codes(Codes)``: WCNF source
- ``file(Path)``: a WCNF file; its contents are read and passed to the
  solver as a file named with the ``.wcnf`` extension, as RC2 and Z3
  select the input format from the file name extension

For ``codes/1`` and ``file/1`` sources, variable terms are recovered
from ``c v Number Term`` marker comments, which are embedded by default
in the output of ``program/1-3`` (use ``embed_variables(false)`` to omit
them). Without markers, model literals use the solver variable numbers.

Options
-------

- ``solver(Solver)``: ``open_wbo`` (default), ``rc2``, or ``z3``
- ``executable(Path)``: solver executable (default: the solver default
  executable, resolved using the system path)
- ``time_limit(Seconds)``: time limit, using the solver native option
  (``-cpu-lim`` for Open-WBO, ``-T`` for RC2, and ``-T:`` for Z3); zero
  (the default) means unlimited
- ``anytime(Boolean)``: whether to accept the best-known solution of a
  run that stops before proving optimality (default: ``false``)
- ``wcnf_format(Format)``: ``old`` (default; ``p wcnf`` header with a
  top weight) or ``new`` (MaxSAT Evaluation format with ``h`` lines and
  no header). The new format is accepted by RC2 and Z3 but not by
  Open-WBO 2.1
- ``embed_variables(Boolean)``: embed variable marker comments in
  ``program/1-3`` output (default: ``true``)

Results
-------

The ``solve/2-3`` predicates return a
``max_sat_result(Status, Model, Properties)`` term where ``Status`` can
be:

- ``optimal``: the cost is optimal; ``Model`` is
  ``assignment(Literals)``, or ``none`` if the solver did not report a
  model
- ``satisfiable``: a best-known solution of a run that stopped early;
  only returned when ``anytime(true)`` (otherwise the status is
  ``unknown``)
- ``unsatisfiable``: the hard clauses cannot be satisfied
- ``unknown``: no result (e.g., time limit reached); ``Model`` is
  ``none``

Properties include ``cost/1`` (when known), ``solver/1``,
``termination/1`` (``completed``, ``resource_limit``, or
``interrupted``), ``anytime/1``, ``variables/1``, ``variable_count/1``,
``diagnostics/1``, ``source_map/1``, ``raw_status/1``,
``process_status/1``, and ``raw_output/1``.

How each solver behaves when a time limit is set:

- Open-WBO is an anytime solver: it reports its best solution so far
  (``satisfiable`` with ``anytime(true)``)
- RC2 is core-guided and reports no solution before proving optimality
- Z3 reports just ``timeout``

Limitations
-----------

- Weights must be positive integers. Costs, weights, and the top weight
  can be arbitrarily large integers on backends with unbounded integers.
- Z3 does not print variables that it eliminates; variables missing from
  its model are reported as false.
- Open-WBO exits with 30 when an optimum is found and 40 when it reports
  a best-known solution. Backends that cannot normalize nonzero exit
  codes (e.g., GNU Prolog) report them as ``unknown(Code)``; the library
  accepts such codes when they are valid for the solver.
