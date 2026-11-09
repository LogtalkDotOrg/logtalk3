.. _library_clingo:

.. _library_clingo:

.. _library_clingo:

``clingo``
==========

The ``clingo`` library provides external CLI solving, ASP source
generation, an opt-in object expansion hook, shown-symbol parsing, and
collected Clingo JSON result inspection.

This library interacts with the Clingo CLI using the ``process``
library. Therefore, it currently supports ECLiPSe, GNU Prolog, SICStus
Prolog, Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Requirements
------------

Solving requires the ``clingo`` executable, available on PATH or
selected using the ``executable(Path)`` option. Clingo 5.5.2 or a later
version is required. Source generation and result inspection do not
require the executable. Clingo is available from its `official
website <https://potassco.org/clingo/>`__.

On POSIX systems, solving also requires ``/bin/sh``, with ``mktemp``
available on PATH. On Windows, solving requires PowerShell 7.3 or later
(``pwsh`` on PATH).

The bundled ``clingo_process.sh`` and ``clingo_process.ps1`` launcher
scripts must remain alongside the library source files.

API documentation
-----------------

Open the
`../../apis/library_index.html#clingo <../../apis/library_index.html#clingo>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(character_sets(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(character_sets(tester)).

The tests include source-generation goldens, symbol parsing, JSON
decoding, expansion isolation and variable sharing, source/result
accessors, and solving. Solver integration has been tested with Clingo
5.8.3. Solver tests are conditional on Clingo availability. A
deterministic shell fixture generates 128 KiB on each output channel to
check file-backed capture beyond typical pipe capacity. Lifecycle checks
verify removal of owned directories after successful execution, child
failure, and solver errors.

The SAT, UNSAT, optimization, and time-limit JSON fixtures were captured
from Clingo 5.8.3 with ``--outf=2 --quiet=0 - 0``; formatting was
compacted without changing the values. The invalid-value and truncated
fixtures are fixed malformed inputs.

Problem fixtures are written as Logtalk objects implementing
``clingo_problem_protocol``. The tester loads them with
``hook(clingo_expansion)`` and solves them using ``object(Object)``,
checking conversion into ASP source and complete CLI solving rather than
passing original native ASP files:

- ``test_files/queens.lgt`` defines ``clingo_queens``, a Logtalk
  adaptation of the official Clingo repository's `queens
  encoding <https://github.com/potassco/clingo/blob/master/examples/gringo/queens/queens1.lp>`__.
  Row choices and attack constraints replace its propagation
  auxiliaries. The test overrides the board size and checks both
  four-queens solutions.
- ``test_files/vertex_cover.lgt`` defines ``clingo_vertex_cover``,
  adapting the official `vertex-cover
  encoding <https://github.com/potassco/clingo/blob/master/examples/clingo/cover/cover.lp>`__,
  replacing Python callback inputs with Logtalk graph facts. The test
  checks the unique minimum cover of a three-vertex path. Both adapted
  fixtures retain the MIT license notice.
- ``test_objects.lgt`` defines ``clingo_team_selection``, selecting a
  skilled team subject to a budget and prioritized objectives. Its test
  checks the exact optimal cost vector and every shown symbol.

The team-selection object covers facts, rules, constraints, constants,
bounded and unbounded choices, disjunctive heads, default and double
negation, strong negation, conditional literals, all six comparison
operators, and the five aggregate forms. It also covers every documented
arithmetic operator, intervals, anonymous and named variables, nested
functions, escaped strings, empty, singleton, and multi-element tuples,
and the infimum and supremum symbols. Output uses empty, signed and
unsigned signature, and conditional term show directives. Optimization
uses minimize, maximize, and weak constraints at distinct priorities.
The object passes a native ``#program base`` directive through
``asp_raw/1``, exercising trusted raw source too.

Usage
-----

Load the library, then compile an object defining a Clingo ASP problem
using the ``clingo_expansion`` expansion hook. For example:

::

   | ?- logtalk_load(clingo(loader)),
        logtalk_load(clingo('test_files/queens'), [hook(clingo_expansion)]).

To inspect the generated ASP source without running Clingo, use the
``clingo::program/1`` predicate. For example:

::

   | ?- clingo::program(object(clingo_queens)).

Use the ``clingo::solve/3`` predicate to solve the ASP problem. For
example, to run Clingo with the board size overridden to four,
collecting both solutions in ``Result`` (generating the source
separately is not required):

::

   | ?- clingo::solve(
           object(clingo_queens),
           Result,
           [models(0), constants([n-number(4)])]
        ).

See the next section for full details on the solver features and
options.

Solving
-------

The public source and result predicates are declared in
`clingo_protocol.lgt <clingo_protocol.lgt>`__. The object data-provider
interface is declared in
`clingo_problem_protocol.lgt <clingo_problem_protocol.lgt>`__. The
serializer, symbol parser, JSON decoder, and expansion hook document
their interfaces in their respective source files.

The ``solve/2`` predicate solves a source with default options. The
``solve/3`` predicate accepts an explicit final options list. Both
predicates return a ground collected result after subprocess completion
and resource cleanup; UNSAT is a successful result, not predicate
failure.

The supported options are:

- ``executable(Path)`` selects a single executable, not a shell command
  line. The default is ``clingo``, resolved through PATH. Existing
  executable paths are used directly.
- ``models(Count)`` selects a nonnegative model limit; the default is
  ``1``. ``0`` requests all models. Optimization may emit multiple
  improving witnesses even when the model limit is nonzero.
- ``time_limit(Seconds)`` selects Clingo's nonnegative solver time
  limit; the default is ``0``, meaning unlimited.
- ``opt_mode(Mode)`` selects ``opt``, ``optN``, or ``ignore``; the
  default is ``opt``. ``optN`` additionally enumerates optimal models
  after proving an optimum.
- ``statistics(Level)`` selects ``0``, ``1``, or ``2``; the default is
  ``0``.
- ``constants(Pairs)`` supplies ``Name-Term`` pairs, using canonical
  ground terms; the default is ``[]``. Each pair becomes a separate CLI
  constant argument.

Shared ``options`` semantics apply: repeated options are accepted and
the first occurrence wins.

For a compiled ``reachability`` object, the ``solve/3`` predicate
returns all models:

::

   | ?- clingo::solve(object(reachability), Result, [models(0)]).

Generated ASP source and solver output use UTF-8. Existing native source
files are left untouched, and relative includes are resolved by Clingo
relative to the source file. Temporary input and output files created
for solving are removed before returning the result.

Shell dependencies that cannot be resolved raise an existence error.
Launcher failures raise ``clingo_transport(Stage, Status, Details)``
errors. Invalid JSON, fatal solver exit statuses, and inconsistent
solver metadata raise
``clingo_solver_error(Formal, Status, Diagnostics, SourceMap)`` errors,
retaining the captured diagnostic codes and object source map.

Sources
-------

The ``program/2`` predicate returns ASP character codes without invoking
Clingo. Its first argument is a ground source envelope:

- ``object(Object)`` enumerates the object's ordered ``asp_statement/2``
  clauses. The object must conform to ``clingo_problem_protocol``.
- ``terms(Statements)`` serializes an ordered list of canonical, ground
  statements.
- ``codes(Codes)`` validates and returns native ASP character codes
  unchanged.
- ``file(Path)`` reads the named native ASP file; ``Path`` must be an
  atom.

The ``print_program/1`` predicate validates and prints a character-code
list to the current output stream, preserving its layout without adding
a newline. It does not parse or ground the ASP source and does not
invoke Clingo.

The ``program/1`` predicate generates and prints ASP source, equivalent
to calling ``program/2`` followed by ``print_program/1``. It accepts the
same source envelopes.

ASP variables in canonical data are explicit ``variable(Name)`` or
``anonymous`` terms, not unbound Logtalk variables. Invalid structures
raise documented errors. Grounding safety, arithmetic semantics, and
satisfiability belong to Clingo.

The ``clingo_program::serialize/2`` predicate serializes canonical
statements. The ``clingo_program::serialize/4`` predicate also accepts a
parallel list of origins and returns a source map. Origins are
``source(File, Start, End)`` or ``unknown``. Map entries are
``line(Start, End, Origin)``, identifying inclusive, one-based generated
line intervals.

Canonical representation
------------------------

The serializer accepts these statement constructors:

- ``fact(Atom)`` emits a fact.
- ``rule(Head, Body)`` emits a rule; ``constraint(Body)`` emits a
  headless rule.
- ``constant(Name, Term)`` emits a constant definition.
- ``show(signature(Name, Arity, Sign))`` selects a predicate signature.
- ``show(none)`` emits an empty show directive; no show statements
  retain Clingo's default visibility.
- ``show_term(Term, Body)`` emits a conditional show directive.
- ``minimize(Elements)`` and ``maximize(Elements)`` emit objective
  directives.
- ``weak_constraint(Body, Weight, Priority, Tuple)`` emits a weak
  constraint.
- ``raw(Codes)`` preserves trusted native ASP text with a separating
  newline.

Atoms use ``function(Name, Arguments, Sign)``, where ``Sign`` is
``positive`` or ``negative``. Rule heads use ``atom(Atom)``,
``disjunction(Atoms)``, or ``choice(Lower, Elements, Upper)``. A choice
bound is a term or ``none``; choice elements use
``conditional(Atom, Body)``.

Bodies are ordered lists of atoms, ``not(Literal)``, comparisons,
conditional literals, and aggregates. Comparisons use
``compare(Operator, Left, Right)`` with ``=``, ``!=``, ``<``, ``<=``,
``>``, or ``>=``. Default negation and strong negation are distinct.

Aggregates use ``aggregate(Function, Elements)`` with ``count``,
``sum``, ``sum_plus``, ``min``, or ``max``; elements use
``aggregate_element(Tuple, Body)``. Objective elements use
``objective(Weight, Priority, Tuple, Body)``. Conditional bodies contain
simple literals.

Terms use ``number(Integer)``, ``string(Codes)``,
``function(Name, Arguments, Sign)``, ``tuple(Terms)``, ``infimum``,
``supremum``, ``variable(Name)``, ``anonymous``,
``interval(Lower, Upper)``, or ``arithmetic(Operator, Arguments)``.
Arithmetic supports unary ``abs`` and ``-``, and binary ``+``, ``-``,
``*``, ``/``, ``\``, and ``**``. Expressions are serialized for
evaluation by Clingo.

The ``clingo::program/2`` predicate returns the ASP source for this
canonical fact:

::

   | ?- clingo::program(terms([
            fact(function(edge, [function(a, [], positive), function(b, [], positive)], positive))
        ]), Codes).

Object syntax
-------------

Compile an ASP container with ``hook(clingo_expansion)`` and declare
that it implements ``clingo_problem_protocol``. For example:

::

   :- object(reachability,
       implements(clingo_problem_protocol)).

       edge(a, b).
       edge(b, c).
       reach(Start, End) :-
           edge(Start, End).
       reach(Start, End) :-
           edge(Start, Middle),
           reach(Middle, End).

       :- show(reach/2).

   :- end_object.

The hook transforms clauses into ordered ``asp_statement/2`` data,
preserving source variable names and sharing. Unnamed underscores become
``anonymous``; named underscore variables remain shared. ASP bodies are
not executable Logtalk goals. Unrelated objects are unchanged. Keep
ordinary runtime helpers in separate provider objects instead of mixing
them into ASP containers.

Equivalent directives include ``const/2``, ``show/1-2``, ``minimize/1``,
``maximize/1``, ``constraint/1``, ``weak_constraint/4``, and
``asp_raw/1``. The surface syntax uses ``not/1`` for default negation
and ``neg/1`` for strong negation. Choice, disjunction, aggregate, and
objective wrappers correspond to the canonical constructors. Native ASP
syntax that is not valid Logtalk syntax belongs in ``file/1``,
``codes/1``, or ``asp_raw/1``.

Unconditional choice and disjunction heads can be written as bodyless
clauses:

::

   choice(size, [conditional(selected(Person), person(Person))], size).
   disjunction([onsite, remote]).

The hook translates these wrappers into ASP rules with empty bodies; an
explicit ``:- true`` is unnecessary. Ordinary atom facts retain their
canonical ``fact(Atom)`` representation.

Raw ASP source
--------------

Raw ASP is Clingo source text supplied without translation into the
canonical representation. Use it for syntax understood by the installed
Clingo version but not represented by the generator. For example,
chained comparisons require Clingo 5.6 or later and can be written
directly in raw source:

::

   item(1..3).
   chosen(X) :- item(X), 1 < X < 3.
   #show chosen/1.

The ``codes(Codes)`` source envelope passes a complete program as
character codes. For example, convert an atom containing the source into
codes and solve it:

::

   | ?- atom_codes('item(1..3). chosen(X) :- item(X), 1 < X < 3. #show chosen/1.', Codes),
        clingo::solve(codes(Codes), Result).

The ``file(Path)`` source envelope passes an existing ASP file directly
to Clingo. For a file named ``problem.lp`` containing the program above:

::

   | ?- clingo::solve(file('problem.lp'), Result).

Neither form requires the ``clingo_expansion`` hook. Both solve the
complete raw program, rather than adding text to another source
envelope.

The ``asp_raw/1`` directive embeds raw source among translated clauses
in an object compiled with ``hook(clingo_expansion)``. Its argument
accepts an atom, a list of character codes, a list of one-character
atoms, or a backend-native string. Use an atom or a double-quoted text
for readable source: the hook handles these representations and
normalizes the text into character codes without changing the backend's
``double_quotes`` flag. The argument is literal text, not an executable
goal. The following object mixes ordinary facts with a raw rule:

::

   :- object(raw_comparison,
       implements(clingo_problem_protocol)).

       item(1).
       item(2).
       item(3).

       :- asp_raw("chosen(X) :- item(X), 1 < X < 3.").
       :- show(chosen/1).

   :- end_object.

After compiling this object with the expansion hook, inspect or solve it
using ``clingo::program(object(raw_comparison))`` or
``clingo::solve(object(raw_comparison), Result)``. The hook preserves
the raw rule at the directive's position without translating its
variable names or operators.

The canonical ``raw(Codes)`` statement provides the same escape hatch
within a ``terms(Statements)`` source or an object's ``asp_statement/2``
data. For example, combine a generated fact with a raw rule and show
directive:

::

   | ?- atom_codes('chosen(X) :- item(X), 1 < X < 3. #show chosen/1.', RawCodes),
        clingo::solve(terms([
            fact(function(item, [number(2)], positive)),
            raw(RawCodes)
        ]), Result).

Raw fragments must contain complete ASP statements, including their
required terminators. The serializer appends a newline after each
``raw(Codes)`` statement, including those produced by ``asp_raw/1``. It
does not parse or validate the ASP syntax inside a fragment; Clingo
checks syntax, grounding safety, and semantics. ASP variables in raw
text are Clingo variables, not shared Logtalk variables. Raw source
remains trusted input, with the include and security limitations
described below.

Collected results
-----------------

The ``clingo_output::decode/6`` predicate consumes a complete CLI JSON
document, raw and normalized process statuses, diagnostic character
codes, and a source map. It returns a ground collected result with this
representation:

::

   clingo_result(Status, Models, Properties)

``Status`` is ``satisfiable``, ``unsatisfiable``, or ``unknown``.
Witnesses containing a ``Value`` array produce records of this form:

::

   model(Symbols, Costs, RawValues)

``Symbols`` contains parsed ground terms; ``RawValues`` preserves the
shown atoms. ``Costs`` is a cost vector or ``none`` when absent. An
empty shown model is a model with empty symbol and raw-value lists, not
an absent witness. Missing optional witness collections can yield no
collected models even when the solver reports satisfiability.
Bound/progression-only records remain in the raw JSON and do not create
models.

The ``model/2`` predicate enumerates collected model records. The
``result_property/2`` predicate enumerates ``status(Status)`` and stored
properties:

- ``solver(Solver)``, ``raw_status(Status)``, and
  ``process_status(Status)`` retain solver and subprocess metadata.
- ``termination(Reason)`` distinguishes ``completed``, ``time_limit``,
  and ``interrupted``.
- ``enumeration_complete(Value)`` is ``true``, ``false``, or
  ``unknown``.
- ``optimality(Value)`` is ``proven``, ``unproven``, or
  ``not_applicable``.
- ``model_count(Count)`` retains the solver summary count or
  ``unknown``.
- ``costs(Costs)`` retains summary costs or ``none``.
- ``time(Time)`` and ``statistics(Statistics)`` retain optional JSON
  values or ``none``.
- ``diagnostics(Codes)``, ``source_map(Map)``, and ``raw_json(JSON)``
  preserve supplied diagnostics, source locations, and all original JSON
  fields.

Satisfiability does not imply complete enumeration or proven optimality.
Improving optimization witnesses are retained with their costs; a proven
global optimum does not make every retained witness optimal. Truncated
JSON, unsupported shown symbols, fatal process statuses, and
inconsistent solver metadata raise errors instead of fabricated results.

The accessors never start or restart a subprocess on backtracking.

The ``print_result/1`` predicate prints a collected result to the
current output stream using the layout illustrated in the `official
Potassco
guide <https://github.com/potassco/guide/blob/master/quickstart.tex>`__.
Each stored witness has an ``Answer:`` header, followed by its original
shown symbols on one line and an ``Optimization:`` cost vector when
present. Empty shown models retain their answer header and an empty
symbol line.

For example, solve a program and print its result:

::

   | ?- atom_codes('{p}.', Codes),
        clingo::solve(codes(Codes), Result, [models(0)]),
        clingo::print_result(Result).

The final status is ``SATISFIABLE``, ``UNSATISFIABLE``, or ``UNKNOWN``,
with ``OPTIMUM FOUND`` used only when a satisfiable result has proven
optimality. The summary prints the solver's model count, appending ``+``
for incomplete enumeration or ``?`` when completeness is unknown.
Unknown counts are printed as ``unknown``. Optimization, interruption,
and total/CPU time metadata are printed when available. Witnesses remain
in their collected order; a proven global optimum does not label earlier
improving witnesses as optimal.

Printing uses only the collected data and never invokes Clingo. It does
not reproduce live progress messages or dump raw JSON, source maps, or
statistics.

Limitations
-----------

The implementation integrates only with the Clingo CLI. It does not use
the Python API, C API, foreign bindings, or a compiled helper.
Protocol-critical JSON and witness-output arguments are fixed; arbitrary
CLI argument passthrough is not supported. Solver stdin is closed, so
there is no interactive input channel or multi-shot controller
interface.

The canonical representation and expansion hook support the documented
ASP syntax, not every native Clingo extension. Conditional bodies accept
only simple literals, not nested conditional literals or aggregates.
Constants do not accept variables or anonymous terms. Native syntax
outside this representation requires ``file/1``, ``codes/1``, or
``asp_raw/1``.

Source inspection does not ground a program, resolve includes, or change
directories. Generated source has no implicit relative-include base. The
``program/2`` predicate does not return a source map; use
``clingo_program::serialize/4`` when one is required. Raw ASP is trusted
input, not a sandbox: includes, embedded scripts, and other native
Clingo facilities can access external resources.

Results and diagnostics are collected in memory, not streamed live.
Exhaustive model enumeration can require substantial memory, and
captured stdout and stderr also require temporary disk space. Shown
integers are bounded by the backend's representable integer range;
values outside that range raise a representation error.

The ``time_limit/1`` option is a solver time limit, not a hard
end-to-end timeout. Cleanup is not guaranteed after non-catchable aborts
or failed OS cleanup operations. An allocation interrupted before its
returned path can be recovered may leave an unidentified directory;
paths with uncertain ownership are not deleted. On Windows, directory
permissions inherit the temporary base's ACL, so use a protected,
user-owned temporary base.

The problem tests cover the syntax represented by the library; they do
not certify all native extensions, embedded scripting, custom theories,
or multi-shot controller workflows.

Compatibility with supported Clingo versions other than 5.8.3 has not
been runtime-verified. Raw ASP supplied through these escape hatches may
require a newer Clingo version than the generated ASP.
