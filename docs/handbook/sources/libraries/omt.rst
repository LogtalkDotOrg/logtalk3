.. _library_omt:

``omt``
=======

The ``omt`` library provides process-backed optimization modulo
theories, SMT-LIB 2 source generation, object expansion, and collected
results. It uses the ``smt`` library's statement vocabulary and process
transport.

As the ``smt`` library dependency interacts with the solvers CLIs using
the ``process`` library, this library currently supports ECLiPSe, GNU
Prolog, SICStus Prolog, Shumway Prolog, SWI-Prolog, Trealla Prolog, and
XVM.

Requirements
------------

Solving requires either Z3 (5.3.0 or later) or OptiMathSAT (1.7.0 or
later). For installation instructions, see:

- https://github.com/Z3Prover/z3
- https://optimathsat.disi.unitn.it

Due to the ``smt`` library dependency, it also requires ``/bin/sh`` and
``mktemp`` on POSIX systems, or PowerShell 7.3 or later on Windows
systems.

API documentation
-----------------

Open the
`../../apis/library_index.html#omt <../../apis/library_index.html#omt>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(omt(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(omt(tester)).

Tests have been run with Z3 5.4.1 and OptiMathSAT 1.7.3. The minimum
versions have not been independently verified.

Problem sources
---------------

An SMT-LIB 2 problem can be represented in several ways:

- ``file(Path)``: ``Path`` must be an atom identifying a native SMT-LIB
  2 file.
- ``codes(Codes)``: ``Codes`` must be a list of character codes
  representing native SMT-LIB 2 source.
- ``terms(Statements)``: ``Statements`` must be an ordered list of
  canonical ground terms representing the problem.
- ``object(Object)``: ``Object`` must be an object containing the
  problem and implementing ``omt_problem_protocol``.

The main library predicates take a ground problem source as an input
argument.

Usage
-----

Load the library, then compile an object defining an optimization
problem using the ``omt_expansion`` hook object. For example:

::

   | ?- logtalk_load(omt(loader)),
        logtalk_load(omt('test_files/test_objects'), [hook(omt_expansion)]).

To inspect the generated SMT-LIB 2 source without running a solver, use
the ``omt::program/1`` predicate. For example:

::

   | ?- omt::program(object(omt_fixture)).

Use the ``omt::solve/2`` predicate to solve the problem. For example, to
solve the ``omt_fixture`` object and inspect its objective bound:

::

   | ?- omt::solve(object(omt_fixture), [Result]),
        omt::objective(Result, x, Bound).

The omt_fixture object contains one check_sat command, so the result
list contains one element.

In alternative, a Logtalk application can dynamically build a list of
canonical statements and solve them through the ``terms(Statements)``
source, without defining an object or loading the hook. For example:

::

   | ?- omt::solve(terms([
            set_logic('QF_LIA'),
            declare_const(x, int),
            assert(>=(x, 2)),
            minimize(x),
            check_sat,
            get_objectives
        ]), [Result]),
        omt::objective(Result, x, Bound).

The ``program/2`` predicate returns SMT-LIB 2 character codes without
invoking a solver.

The ``print_program/1`` predicate validates and prints a character-code
list to the current output stream, preserving its layout without adding
a newline. It does not parse or validate the SMT-LIB 2 syntax and does
not invoke a solver.

The ``program/1`` predicate generates and prints SMT-LIB 2 source,
equivalent to calling ``program/2`` followed by ``print_program/1``. It
accepts the same source envelopes.

See the next section for full details on the solver features and
options.

Solving
-------

The public source and result predicates are declared in the
``omt_protocol`` protocol. The object data-provider interface is
declared in the ``omt_problem_protocol`` protocol. The expansion hook
preserves statement order and source locations.

The ``solve/2`` predicate solves a problem with default options. The
``solve/3`` predicate accepts a list of options. Both predicates return
a ground list of collected results, one per ``check_sat`` or
``check_sat_assuming(Literals)`` statement, in order, after subprocess
completion and resource cleanup. These statements generate the SMT-LIB 2
``check-sat`` and ``check-sat-assuming`` commands, respectively.
UNSATISFIABLE is a successful result, not predicate failure.

Structured sources use the SMT vocabulary plus ``minimize(Term)``,
``maximize(Term)``, and ``get_objectives``. Response-producing commands,
including ``get_objectives`` and ``get_model``, must be requested
explicitly. See the ``smt`` library for the remaining statements, sorts
and terms.

Structured statements allow at most one active objective at a time.
``push/1`` and ``pop/1`` preserve objective scope, so a batch may
perform multiple checks and optimize different objectives in separate
scopes. An unmatched ``pop`` or a second active objective raises a
domain error. Native ``file/1``, ``codes/1``, and trusted ``raw/1``
input is passed through without parsing, so the caller is responsible
for objective scope there.

For example, the following query minimizes ``x`` under the temporary
assumption ``p``, yielding a bound of ``3`` with Z3:

::

   | ?- omt::solve(terms([
            set_logic('QF_LIA'),
            declare_const(x, int),
            declare_const(p, bool),
            assert(>=(x, 0)),
            assert('=>'(p, >=(x, 3))),
            minimize(x),
            check_sat_assuming([p]),
            get_objectives
        ]), [Result]),
        omt::objective(Result, x, Bound).

OptiMathSAT 1.7.3 has been observed to report ``sat`` but an unknown
objective bound for this query. Check the returned bound rather than
assuming that a satisfiable result includes an optimum.

The supported options are:

- ``solver(Solver)`` selects which solver is invoked: ``z3`` (the
  default) or ``optimathsat``. The selected solver determines the
  default ``executable/1`` value and how ``time_limit/1`` is handled.
- ``executable(Path)`` selects a single executable, not a shell command
  line. The default depends on ``solver/1``: ``z3`` for ``z3`` and
  ``optimathsat`` for ``optimathsat``. Existing executable paths are
  used directly; otherwise the executable is resolved through the system
  PATH.
- ``time_limit(Seconds)`` selects a nonnegative integer solver time
  limit in seconds; the default is ``0``, meaning unlimited. For Z3 this
  is passed using ``-T:``. A nonzero limit with OptiMathSAT raises
  ``domain_error(optimathsat_time_limit, Seconds)`` because its
  inspected CLI does not provide an equivalent general timeout.

OptiMathSAT is invoked with optimization and model generation enabled.

Shared ``options`` semantics apply: repeated options are accepted and
the first occurrence wins.

For the compiled ``omt_fixture`` object, the ``solve/2`` predicate
returns the collected results using the default solver (Z3):

::

   | ?- omt::solve(object(omt_fixture), Results).

To solve the same object with OptiMathSAT instead:

::

   | ?- omt::solve(object(omt_fixture), Results, [solver(optimathsat)]).

Results and errors
------------------

Each result returned by ``omt::solve/2-3`` uses the SMT term
representation:

::

   smt_result(Status, Model, Properties)

The term arguments are:

- ``Status``: ``satisfiable``, ``unsatisfiable``, or ``unknown``.
- ``Model``: ``none`` or ``bindings([binding(Symbol, Value), ...])``.
- ``Properties``: a list of solver and process information, including
  ``solver/1``, ``process_status/1``, ``diagnostics/1``,
  ``source_map/1``, and ``raw_output/1``. It also includes ``core/1``
  and ``values/1`` for requested unsatisfiable core and term values.

When the source requests an objective response using ``get_objectives``,
the result properties also include
``objectives([objective(Expression, Bound), ...])``.

The ``omt`` object provides predicates for inspecting a collected result
without running a solver again:

- ``status(Result, Status)`` returns its status.
- ``model_binding(Result, Symbol-Value)`` enumerates its model bindings.
- ``model_value(Result, Symbol, Value)`` looks up a model binding by
  symbol.
- ``unsat_core_name(Result, Name)`` enumerates its unsatisfiable core
  names.
- ``objective(Result, Expression, Bound)`` enumerates its objective
  expressions and reported bounds. It fails when no matching objective
  response exists.
- ``result_property(Result, Property)`` enumerates its status and stored
  properties.

The ``print_result/1`` predicate prints a single result, while
``print_results/1`` prints a list of results separated by blank lines.

For example, after loading the ``omt_fixture`` object as shown in the
"Usage" section, the following query obtains its status and objective
bound:

::

   | ?- omt::solve(object(omt_fixture), [Result]),
        omt::status(Result, Status),
        omt::objective(Result, x, Bound).

For this problem, ``Status`` is ``satisfiable`` and ``Bound`` is ``2``.

Bounds are separate from model values: a feasible model is not
necessarily an attained optimum. Numbers in reported objective
expressions are preserved exactly; decimals use ``decimal(Atom)`` and
other compound expressions preserve the solver's S-expression structure.
The atom ``oo`` represents positive infinity, and ``-(oo)`` represents
negative infinity. Strict bounds may also contain ``epsilon``.
OptiMathSAT's range annotations are represented as
``annotated(Bound, range(Lower, Upper))``. Values and shapes can differ
between solvers. Check status and termination metadata before
interpreting a bound; ``sat``, ``unsat``, or ``unknown`` alone does not
establish that a finite bound was attained.

Missing executables and invalid inputs raise errors. Solver errors,
malformed responses and nonzero or interrupted process status raise
``omt_solver_error(Formal, Status, Diagnostics, SourceMap)``. Transport
errors from the shared SMT launcher use ``smt_transport/3``. Input and
output are UTF-8; temporary process files are removed before returning.

Limitations
-----------

The structured API does not expose multiple simultaneous objectives,
soft constraints, or solver-specific optimization modes. Their supported
theories and treatment of unbounded problems differ. An unbounded real
optimization probe with OptiMathSAT 1.7.3 did not complete promptly; no
library timeout is available for that backend. Each call starts a new
process; there is no persistent session between calls.
