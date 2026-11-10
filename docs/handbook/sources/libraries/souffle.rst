.. _library_souffle:

``souffle``
===========

The ``souffle`` library provides access to the Souffle Datalog engine
CLI, Datalog source generation, an opt-in object expansion hook, and
collected result inspection (decoded ``.output`` relations and
``.printsize`` counts).

This library interacts with the Souffle CLI using the ``process``
library. Therefore, it currently supports ECLiPSe, GNU Prolog, SICStus
Prolog, Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Requirements
------------

Solving requires the ``souffle`` executable, available on PATH or
selected using the ``executable(Path)`` option. This library targets
Souffle 2.5. Souffle is available from its `official
website <https://souffle-lang.github.io/>`__. The ``compile(true)``
option additionally requires a working C++ toolchain (Souffle's ``-c``
flag compiles the generated C++ program before running it).

On POSIX systems, solving also requires ``/bin/sh``, with ``mktemp``
available on PATH. On Windows, solving requires PowerShell 7.3 or later
(``pwsh`` on PATH).

The bundled ``souffle_process.sh`` and ``souffle_process.ps1`` launcher
scripts must remain in the ``scripts`` subdirectory of the library.

API documentation
-----------------

Open the
`../../apis/library_index.html#souffle <../../apis/library_index.html#souffle>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(souffle(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(souffle(tester)).

The tests include source-generation goldens, CSV/printsize decoding,
expansion isolation and variable sharing, source/result accessors, and
solving. Solver integration has been tested with Souffle 2.5 on
SWI-Prolog and SICStus Prolog 4.10.1 on macOS, and verified empirically
against the installed CLI (exit codes, CSV formatting, aggregate syntax,
and escaping) before the serializer and decoder were written. Solver
tests are conditional on the ``souffle`` executable's availability.

Problem fixtures are written as Logtalk objects implementing
``souffle_problem_protocol``. The tester loads them with
``hook(souffle_expansion)`` and solves them using ``object(Object)``,
checking conversion into Datalog source and complete CLI solving:

- ``test_files/reachability.lgt`` defines ``souffle_reachability``, the
  graph reachability example from the `Souffle
  documentation <https://souffle-lang.github.io/simple>`__, with
  ``edge`` as an ``.input`` relation and ``path`` as an
  ``.output``/``.printsize`` relation.
- ``test_files/graph_stats.lgt`` defines ``souffle_graph_stats``,
  covering typed columns (``number``, ``unsigned``, ``float``,
  ``symbol``), negation (``\+``), comparisons, the
  ``count``/``sum``/``max``/``min``/``mean`` aggregates, and the
  ``cat/2`` string-concatenation functor.

Usage
-----

Load the library, then compile an object defining a Datalog problem
using the ``souffle_expansion`` expansion hook. For example:

::

   | ?- logtalk_load(souffle(loader)),
        logtalk_load(souffle('test_files/reachability'), [hook(souffle_expansion)]).

To inspect the generated Datalog source without running Souffle, use the
``souffle::program/1`` predicate:

::

   | ?- souffle::program(object(souffle_reachability)).

Use the ``souffle::solve/3`` predicate to solve the problem. The
``outputs/1`` option is always required to decode ``.output`` relations:
it lists which relations to read back and their column types, since
tab-separated CSV output carries no embedded type information. For
example:

::

   | ?- souffle::solve(
            object(souffle_reachability),
            Result,
            [facts_data([edge-[[1,2],[2,3]]]), outputs([path-[number,number]])]
        ),
        souffle::relation(Result, path, _, Rows).

See the next section for full details on the options.

Solving
-------

The public source and result predicates are declared in
`souffle_protocol.lgt <souffle_protocol.lgt>`__. The object
data-provider interface is declared in
`souffle_problem_protocol.lgt <souffle_problem_protocol.lgt>`__. The
serializer, CSV/printsize decoder, and expansion hook document their
interfaces in their respective source files.

The ``solve/2`` predicate solves a source with default options. The
``solve/3`` predicate accepts an explicit final options list. Both
predicates return a ground collected result after subprocess completion
and resource cleanup.

The supported options are:

- ``executable(Path)`` selects a single executable, not a shell command
  line. When omitted, the executable name is ``souffle``, resolved
  through PATH. Existing executable paths are used directly.
- ``jobs(Count)`` selects a positive parallelism degree or ``auto``; the
  default is ``1``.
- ``warnings(Bool)`` disables Souffle warnings (``-w``) when ``false``;
  the default is ``true``.
- ``compile(Bool)`` compiles the generated C++ program before running it
  (``-c``) when ``true``; the default is ``false`` (interpreter mode).
- ``facts(Path)`` selects an existing external directory of ``.facts``
  files, passed directly as the Souffle fact directory; the default is
  ``none``.
- ``facts_data(Pairs)`` supplies ``Relation-Rows`` pairs (``Rows`` a
  list of row value lists; integers, floats, and atoms for
  ``number``/``unsigned``, ``float``, and ``symbol`` columns
  respectively), materialized as ``.facts`` files in an owned temporary
  directory; the default is ``[]``. ``facts/1`` and ``facts_data/1`` are
  mutually exclusive: supplying both with non-default values raises
  ``domain_error(souffle_facts_option, ...)``.
- ``outputs(Pairs)`` supplies ``Relation-Types`` pairs (``Types`` a list
  of column type atoms, one of ``number``, ``unsigned``, ``float``, or
  ``symbol``) listing which ``.output`` relations to decode and their
  column types; the default is ``[]`` (nothing decoded). This option is
  always required, regardless of source kind, since tab-separated CSV
  output carries no embedded type information.
- ``printsizes(Names)`` supplies relation names whose ``.printsize``
  count to decode from captured stdout; the default is ``[]``.

Shared ``options`` semantics apply: repeated options are accepted and
the first occurrence wins.

A declared ``.input`` relation with no matching ``.facts`` file (neither
supplied via ``facts/1``/``facts_data/1`` nor otherwise present in the
fact directory) is silently treated as an empty relation only if that
relation is otherwise unused (not read by any rule or ``.output``). If a
rule actually reads it, a missing ``.facts`` file is a Souffle error
(surfaced as ``souffle_solver_error/4``), not an empty relation. This is
Souffle's own behavior, not a limitation of this library.

Generated Datalog source is written using UTF-8. Existing native source
files are left untouched. Temporary input, fact, and output files and
directories created for solving are removed before returning the result.

Shell dependencies that cannot be resolved raise an existence error.
Launcher failures raise ``souffle_transport(Stage, Status, Details)``
errors. Invalid CSV content, a missing requested relation file, and a
nonzero Souffle exit status raise
``souffle_solver_error(Formal, Status, Diagnostics, SourceMap)`` errors,
retaining the captured diagnostics and object source map.

Sources
-------

The ``program/2`` predicate returns Datalog character codes without
invoking Souffle. Its first argument is a ground source envelope:

- ``object(Object)`` enumerates the object's ordered
  ``souffle_statement/2`` clauses. The object must conform to
  ``souffle_problem_protocol``.
- ``terms(Statements)`` serializes an ordered list of canonical, ground
  statements.
- ``codes(Codes)`` and ``file(Path)`` pass through native Datalog source
  unchanged; Souffle parses and reports any errors directly.

Statement vocabulary
--------------------

Canonical statements (see
`souffle_program.lgt <souffle_program.lgt>`__):

- ``decl(Name, Columns)``: a ``.decl`` declaration, ``Columns`` a list
  of ``ColumnName-Type`` pairs (``Type`` one of ``number``,
  ``unsigned``, ``float``, ``symbol``).
- ``input(Name)``, ``output(Name)``, ``printsize(Name)``: ``.input``,
  ``.output``, and ``.printsize`` directives.
- ``fact(Atom)``, ``rule(Head, Body)``: ``Atom``/``Head`` a
  ``function(Name, Arguments)`` term; ``Body`` a list of literals (a
  positive ``function(Name, Arguments)`` term, ``not(Atom)`` for
  negation, or ``compare(Operator, Left, Right)`` for ``=``, ``!=``,
  ``<``, ``<=``, ``>``, ``>=`` comparisons, where either operand can be
  an ``aggregate(Function, TargetExpr, Body)`` term, ``Function`` one of
  ``count``, ``sum``, ``max``, ``min``, ``mean``).
- Term arguments: ``number(Integer)``, ``unsigned(Integer)``,
  ``float(Float)``, ``string(Codes)`` (a quoted symbol literal),
  ``variable(Name)``, ``anonymous``,
  ``arithmetic(Operator, [Left, Right])`` for ``+``, ``-``, ``*``,
  ``/``, ``%``, and ``cat(Left, Right)`` (the built-in
  string-concatenation functor).
- ``comment(Codes)``, ``raw(Codes)``: a ``//`` line comment, or trusted
  raw Datalog source text passed through unchanged.

Using the ``souffle_expansion`` hook with an object implementing
``souffle_problem_protocol``, ordinary Prolog-like facts and rules are
translated automatically, and the following directives are also
recognized: ``:- souffle_decl(Name, Columns)``,
``:- souffle_input(Name)``, ``:- souffle_output(Name)``,
``:- souffle_printsize(Name)``, ``:- souffle_raw(Source)``, and
``:- comment(Source)``. The ``mean`` aggregate requires its target
expression to already resolve to a ``float``-typed value; Souffle does
not implicitly widen ``number`` to ``float`` for ``mean``.

Known limitations
-----------------

- No custom ``.type`` declarations (type aliases, records, unions, or
  ADTs); column types are restricted to the four Souffle primitives.
- No components, choice construct, subsumption, user-defined functors,
  provenance (``-t``), profiling (``-p``), debug reports (``-r``),
  SQLite or stdin I/O, compressed or RFC 4180 CSV I/O, per-relation
  custom filename/delimiter overrides, or the legacy
  ``input``/``output`` qualifier syntax.
- No ``.limitsize`` directive and no solver-native time limit: Souffle
  has no CLI time-limit flag; impose one externally (for example, at the
  operating-system level) if needed.
- Souffle's default (non RFC 4180) tab-separated CSV output does not
  escape symbol values at all: a symbol value containing a literal tab
  byte breaks column alignment on decoding. Avoid such data, or
  post-process affected columns externally.
