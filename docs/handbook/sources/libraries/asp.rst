.. _library_asp:

``asp``
=======

The ``asp`` library provides access to Clingo and Zetesis CLI solving,
ASP source generation, an opt-in object expansion hook, shown-symbol
decoding, and collected result inspection. The default solver is Clingo.

This library interacts with the solver CLIs using the ``process``
library. Therefore, it currently supports ECLiPSe, GNU Prolog, SICStus
Prolog, Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Requirements
------------

Solving requires the selected solver executable, available on the system
``PATH`` or selected using the ``executable(Path)`` option. Clingo
requires version 5.5.2 or later; older versions can emit incorrectly
escaped JSON strings. Zetesis requires version 0.7.0 with schema-3 JSON
output. Note that source generation and result inspection do not require
a solver executable. For solver documentation and installation
instructions, see:

- https://potassco.org/clingo/
- https://github.com/GregoryGelfond/zetesis

On POSIX systems, solving also requires ``/bin/sh``, with ``mktemp``
available on the system ``PATH``. On Windows, solving requires
PowerShell 7.3 or later (``pwsh`` on the system ``PATH``).

The bundled ``asp_process.sh`` and ``asp_process.ps1`` launcher scripts
must remain in the ``scripts`` subdirectory of the library.

API documentation
-----------------

Open the
`../../apis/library_index.html#asp <../../apis/library_index.html#asp>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(asp(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(asp(tester)).

The tests include source-generation goldens, symbol parsing, JSON
decoding, expansion isolation and variable sharing, source/result
accessors, and solving. Solver integration has been tested with Clingo
5.8.3 and Zetesis 0.7.0. Solver tests are conditional on the required
executable's availability. A deterministic shell fixture generates 128
KiB on each output channel to check file-backed capture beyond typical
pipe capacity. Lifecycle checks verify removal of owned directories
after successful execution, child failure, and solver errors.

The SAT, UNSAT, optimization, and time-limit JSON fixtures were captured
from Clingo 5.8.3 with ``--outf=2 --quiet=0 - 0``; formatting was
compacted without changing the values. Zetesis fixtures were captured
from its installed 0.7.0 CLI with ``solve --json --threads 1``. The
malformed fixtures are fixed inputs. Both solvers solve the
reachability, four-queens, vertex-cover, and team-selection problems.
Comparisons use normalized model multisets and cost vectors, not
solver-dependent answer ordering or optimization history. Zetesis checks
also cover typed symbols, hidden model multiplicity, unknown tie counts,
interruption, solver-specific option rejection, and cleanup after syntax
failures.

Problem fixtures are written as Logtalk objects implementing
``asp_problem_protocol``. The tester loads them with
``hook(asp_expansion)`` and solves them using ``object(Object)``,
checking conversion into ASP source and complete CLI solving rather than
passing original native ASP files:

- ``test_files/queens.lgt`` defines ``asp_queens``, a Logtalk adaptation
  of the official Clingo repository's `queens
  encoding <https://github.com/potassco/clingo/blob/master/examples/gringo/queens/queens1.lp>`__.
  Row choices and attack constraints replace its propagation
  auxiliaries. The test overrides the board size and checks both
  four-queens solutions.
- ``test_files/vertex_cover.lgt`` defines ``asp_vertex_cover``, adapting
  the official `vertex-cover
  encoding <https://github.com/potassco/clingo/blob/master/examples/clingo/cover/cover.lp>`__,
  replacing Python callback inputs with Logtalk graph facts. The test
  checks the unique minimum cover of a three-vertex path. Both adapted
  fixtures retain the MIT license notice.
- ``test_objects.lgt`` defines ``asp_team_selection``, selecting a
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
``raw/1``, exercising trusted raw source too.

Problem sources
---------------

An ASP problem can be represented in several ways:

- ``file(Path)``: ``Path`` must be an atom identifying a native ASP
  file.
- ``codes(Codes)``: ``Codes`` must be a list of character codes
  representing native ASP source.
- ``terms(Statements)``: ``Statements`` must be an ordered list of
  canonical ground terms representing the problem.
- ``object(Object)``: ``Object`` must be an object containing the
  problem and implementing ``asp_problem_protocol``.

The main library predicates take a ground problem source as an input
argument.

Usage
-----

Load the library, then compile an object defining an ASP problem using
the ``asp_expansion`` hook object. For example:

::

   | ?- logtalk_load(asp(loader)),
        logtalk_load(asp('test_files/queens'), [hook(asp_expansion)]).

To inspect the generated ASP source without running Clingo, use the
``asp::program/1`` predicate. For example:

::

   | ?- asp::program(object(asp_queens)).

Use the ``asp::solve/3`` predicate to solve the ASP problem. For
example, to run Clingo with the board size overridden to four,
collecting both solutions in ``Result`` (generating the source
separately is not required):

::

   | ?- asp::solve(
            object(asp_queens),
            Result,
            [models(0), constants([n-number(4)])]
        ).

In alternative, a Logtalk application can instead dynamically build a
list of canonical statements and solve them through the
``terms(Statements)`` source, without defining an object or loading the
hook. An example using a trivial single-fact program:

::

   | ?- asp::solve(terms([fact(function(p, [], positive))]), Result).

To select Zetesis for a compiled problem:

::

   | ?- asp::solve(object(asp_vertex_cover), Result, [solver(zetesis), models(0)]).

The ``program/2`` predicate returns ASP character codes without invoking
a solver.

The ``print_program/1`` predicate validates and prints a character-code
list to the current output stream, preserving its layout without adding
a newline. It does not parse or ground the ASP source and does not
invoke a solver.

The ``program/1`` predicate generates and prints ASP source, equivalent
to calling ``program/2`` followed by ``print_program/1``. It accepts the
same source envelopes.

ASP variables in canonical data are explicit ``variable(Name)`` or
``anonymous`` terms, not unbound Logtalk variables. Invalid structures
raise documented errors. Grounding safety, arithmetic semantics, and
satisfiability belong to the selected solver.

The ``asp_program::serialize/2`` predicate serializes canonical
statements. The ``asp_program::serialize/4`` predicate also accepts a
parallel list of origins and returns a source map. Origins are
``source(File, Start, End)`` or ``unknown``. Map entries are
``line(Start, End, Origin)``, identifying inclusive, one-based generated
line intervals.

See the next section for full details on the solver features and
options.

Solving
-------

The public source and result predicates are declared in the
``asp_protocol`` protocol. The object data-provider interface is
declared in the ``asp_problem_protocol`` protocol. The serializer,
symbol parser, JSON decoder, and expansion hook document their
interfaces in their respective source files.

The ``solve/2`` predicate solves a source with default options. The
``solve/3`` predicate accepts an explicit final options list. Both
predicates return a ground collected result after subprocess completion
and resource cleanup; UNSAT is a successful result, not predicate
failure.

The supported options are:

- ``solver(Solver)`` selects ``clingo`` or ``zetesis``; the default is
  ``clingo``.
- ``executable(Path)`` selects a single executable, not a shell command
  line. When omitted, the executable name is the selected solver,
  resolved through PATH. Existing executable paths are used directly.
- ``models(Count)`` selects a nonnegative model limit; the default is
  ``1``. ``0`` requests all models. Clingo optimization may emit
  multiple improving witnesses even with a nonzero limit. Zetesis bounds
  published answers; with optimization, ``0`` requests all optimal ties
  rather than improving witnesses.
- ``time_limit(Seconds)`` selects a nonnegative solver time limit; the
  default is ``0``, meaning unlimited.
- ``opt_mode(Mode)`` selects ``opt``, ``optN``, or ``ignore``; the
  default is ``opt``. ``optN`` additionally enumerates optimal models
  after proving an optimum.
- ``statistics(Level)`` selects ``0``, ``1``, or ``2``; the default is
  ``0``. Zetesis maps either nonzero level to ``--stats``.
- ``constants(Pairs)`` supplies ``Name-Term`` pairs, using canonical
  ground terms; the default is ``[]``. Each pair becomes a separate CLI
  constant argument.
- ``backend(Backend)`` selects Zetesis's ``cpu``, ``gpu``, ``metal``, or
  ``vulkan`` backend; the default is ``cpu``. Availability depends on
  the installed solver and hardware; the library does not provide GPU
  implementations.
- ``grounder(Grounder)`` selects Zetesis's ``auto``, ``eager``, or
  ``lazy`` grounder; the default is ``auto``.
- ``threads(Count)`` selects a positive Zetesis thread count or
  ``auto``; the default is ``auto``, omitting the CLI setting.
- ``memory(Bytes)`` selects a positive Zetesis memory budget in bytes or
  ``auto``; the default is ``auto``, omitting the CLI setting. This
  allowance covers named storage capacities, not a hard process RSS cap.

Clingo accepts only the default values of the Zetesis-specific controls.
Zetesis accepts only ``opt_mode(opt)`` and ``constants([])``.
Unsupported effective options raise
``domain_error(asp_solver_option, Solver-Option)`` before launch.
Constant definitions can be supplied in canonical statements or native
source; the library does not emulate Clingo's constant-override option
for Zetesis.

Shared ``options`` semantics apply: repeated options are accepted and
the first occurrence wins.

For a compiled ``reachability`` object, the ``solve/3`` predicate
returns all models:

::

   | ?- asp::solve(object(reachability), Result, [models(0)]).

Generated ASP source and solver output use UTF-8. Existing native source
files are left untouched, and relative includes are resolved by the
selected solver relative to the source file. Temporary input and output
files created for solving are removed before returning the result.

Shell dependencies that cannot be resolved raise an existence error.
Launcher failures raise ``asp_transport(Stage, Status, Details)``
errors. Invalid JSON, fatal solver exit statuses, and inconsistent
solver metadata raise
``asp_solver_error(Formal, Status, Diagnostics, SourceMap)`` errors,
retaining the captured diagnostic codes and object source map.

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
- ``raw(Source)`` preserves trusted native ASP text with a separating
  newline; ``Source`` is an atom, a list of character codes, a list of
  one-character atoms, or a backend-native string, normalized into
  character codes.

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
evaluation by the selected solver.

The ``asp::program/2`` predicate returns the ASP source for this
canonical fact:

::

   | ?- asp::program(terms([
            fact(function(edge, [function(a, [], positive), function(b, [], positive)], positive))
        ]), Codes).

Object syntax
-------------

Compile an ASP container with ``hook(asp_expansion)`` and declare that
it implements ``asp_problem_protocol``. For example:

::

   :- object(reachability,
       implements(asp_problem_protocol)).

       edge(a, b).
       edge(b, c).
       reach(Start, End) :-
           edge(Start, End).
       reach(Start, End) :-
           edge(Start, Middle),
           reach(Middle, End).

       :- show(reach/2).

   :- end_object.

The hook translates this object into the canonical statements:

::

   fact(function(edge, [function(a, [], positive), function(b, [], positive)], positive))
   fact(function(edge, [function(b, [], positive), function(c, [], positive)], positive))
   rule(atom(function(reach, [variable('Start'), variable('End')], positive)), [
       function(edge, [variable('Start'), variable('End')], positive)
   ])
   rule(atom(function(reach, [variable('Start'), variable('End')], positive)), [
       function(edge, [variable('Start'), variable('Middle')], positive),
       function(reach, [variable('Middle'), variable('End')], positive)
   ])
   show(signature(reach, 2, positive))

The hook transforms clauses into ordered ``asp_statement/2`` data,
preserving source variable names and sharing. Unnamed underscores become
``anonymous``; named underscore variables remain shared. ASP bodies are
not executable Logtalk goals. Unrelated objects are unchanged. Keep
ordinary runtime helpers in separate provider objects instead of mixing
them into ASP containers.

Equivalent directives mostly share the functor and arity of their
matching canonical statement constructor (``show/1``, ``minimize/1``,
``maximize/1``, ``constraint/1``, ``weak_constraint/4``, and ``raw/1``),
with two exceptions: ``const/2`` maps to ``constant/2`` and the
two-argument ``show/2`` directive maps to ``show_term/2``. The surface
syntax uses ``not/1`` for default negation and ``neg/1`` for strong
negation. Choice, disjunction, aggregate, and objective wrappers
correspond to the canonical constructors. Native ASP syntax that is not
valid Logtalk syntax belongs in ``file/1``, ``codes/1``, or ``raw/1``.

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

Raw ASP is native ASP source text supplied without translation into the
canonical representation. It can be used with either Clingo or Zetesis,
provided the text uses syntax supported by the selected solver and its
installed version. Solver-specific extensions are not automatically
portable between solvers. Use raw source for syntax not represented by
the generator. For example, the following program uses chained
comparisons, which require Clingo 5.6 or later when solving with Clingo:

::

   item(1..3).
   chosen(X) :- item(X), 1 < X < 3.
   #show chosen/1.

The ``codes(Codes)`` source envelope passes a complete program as
character codes. For example, convert an atom containing the source into
codes and solve it:

::

   | ?- atom_codes('item(1..3). chosen(X) :- item(X), 1 < X < 3. #show chosen/1.', Codes),
        asp::solve(codes(Codes), Result).

The ``file(Path)`` source envelope passes an existing ASP file directly
to the selected solver. For a file named ``problem.lp`` containing the
program above:

::

   | ?- asp::solve(file('problem.lp'), Result).

Neither form requires the ``asp_expansion`` hook. Both solve the
complete raw program, rather than adding text to another source
envelope.

The ``raw/1`` directive works with both Clingo and Zetesis. It embeds
raw source among translated clauses in an object compiled with
``hook(asp_expansion)``. Select the solver using the ``solver(Solver)``
option of ``asp::solve/3``; ``asp::solve/2`` uses Clingo by default. The
``raw/1`` argument accepts an atom, a list of character codes, a list of
one-character atoms, or a backend-native string. Use an atom or a
double-quoted text for readable source: the library handles these
representations and normalizes the text into character codes without
changing the backend's ``double_quotes`` flag. The argument is literal
text, not an executable goal. The following object mixes ordinary facts
with a raw rule:

::

   :- object(raw_comparison,
       implements(asp_problem_protocol)).

       item(1).
       item(2).
       item(3).

       :- raw("chosen(X) :- item(X), 1 < X < 3.").
       :- show(chosen/1).

   :- end_object.

After compiling this object with the expansion hook, inspect or solve it
using ``asp::program(object(raw_comparison))`` or
``asp::solve(object(raw_comparison), Result)``. The hook preserves the
raw rule at the directive's position without translating its variable
names or operators.

The canonical ``raw(Source)`` statement provides the same escape hatch
within a ``terms(Statements)`` source or an object's ``asp_statement/2``
data, accepting the same normalized argument forms as the ``raw/1``
directive. For example, combine a generated fact with a raw rule and
show directive:

::

   | ?- asp::solve(terms([
            fact(function(item, [number(2)], positive)),
            raw("chosen(X) :- item(X), 1 < X < 3. #show chosen/1.")
        ]), Result).

Raw fragments must contain complete ASP statements, including their
required terminators. The serializer appends a newline after each
``raw(Source)`` statement. It does not parse or validate the ASP syntax
inside a fragment; the selected solver checks syntax, grounding safety,
and semantics. ASP variables in raw text are ASP variables, not shared
Logtalk variables. Raw source remains trusted input, with the include
and security limitations described below.

Collected results
-----------------

The ``asp_output::decode/8`` predicate consumes a solver identifier, a
complete CLI JSON document, raw and normalized process statuses,
diagnostic character codes, and a source map. Its final argument is an
options list. It returns a ground collected result with this
representation:

::

   asp_result(Status, Models, Properties)

``Status`` is ``satisfiable``, ``unsatisfiable``, or ``unknown``.
Collected shown models use this representation:

::

   model(Symbols, Costs, RawValues)

``Symbols`` contains canonical ground terms. For Clingo, ``RawValues``
preserves the shown strings from its JSON ``Value`` array. For Zetesis,
``RawValues`` contains generated ASP spellings of its typed shown
symbols, not original source text. Zetesis's incremental atom table,
zero-based indices, and prefix-encoded terms are decoded without
reparsing generated strings. Full-model atoms are retained only in the
raw JSON; the public models contain shown atoms and terms. ``Costs`` is
a cost vector or ``none`` when absent. An empty shown model is a model
with empty symbol and raw-value lists, not an absent witness. Missing
optional Clingo witness collections can yield no collected models even
when it reports satisfiability. Bound/progression-only records remain in
the raw JSON and do not create models. Distinct Zetesis published
records remain distinct even when their shown symbols are identical.

The ``model/2`` predicate enumerates collected model records. The
``result_property/2`` predicate enumerates ``status(Status)`` and stored
properties:

- ``solver(Solver)`` identifies ``clingo`` or ``zetesis``.
  ``solver_version(Version)`` retains Clingo's native version banner or
  ``unknown`` for Zetesis, whose JSON does not supply a version.
  ``raw_status(Status)`` and ``process_status(Status)`` retain
  subprocess metadata.
- ``termination(Reason)`` distinguishes ``completed``, ``time_limit``,
  and ``interrupted``.
- ``enumeration_complete(Value)`` is ``true``, ``false``, or
  ``unknown``.
- ``optimality(Value)`` is ``proven``, ``unproven``, or
  ``not_applicable``.
- ``model_count(Count)`` retains the solver summary count or
  ``unknown``.
- ``verified_model_count(Count)``, ``optimal_tie_count(Count)``, and
  ``cost_priorities(Priorities)`` retain Zetesis verification, tie, and
  descending priority information. Unavailable tie counts are
  ``unknown``, not zero.
- ``costs(Costs)`` retains summary costs or ``none``.
- ``time(Time)`` and ``statistics(Statistics)`` retain optional JSON
  values or ``none``.
- ``diagnostics(Codes)``, ``source_map(Map)``, and ``raw_json(JSON)``
  preserve supplied diagnostics, source locations, and all original JSON
  fields.
- ``search_complete(Value)``, ``publication_complete(Value)``,
  ``completion(Value)``, ``coverage(Value)``, ``interruption(Value)``,
  and ``publication_stop(Value)`` retain Zetesis's distinct search and
  publication evidence. Publication completion does not imply exhaustive
  search. Zetesis time data is ``none`` when unavailable.

Satisfiability does not imply complete enumeration or proven optimality.
Clingo's improving optimization witnesses are retained with their costs;
a proven global optimum does not make every retained witness optimal.
Zetesis publishes retained best models with their priority-aligned
costs; a bounded optimal answer request can prove an optimum while
leaving the tie count and enumeration completeness unknown. Completed
Zetesis runs exit with 0, interrupted runs with 3, and syntax failures
with 2. Truncated JSON, unsupported shown symbols, fatal process
statuses, and inconsistent solver metadata raise errors instead of
fabricated results.

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
        asp::solve(codes(Codes), Result, [models(0)]),
        asp::print_result(Result).

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

The implementation integrates only with the Clingo and Zetesis CLIs. It
does not use the Python API, C API, foreign bindings, or a compiled
helper. Protocol-critical JSON and witness-output arguments are fixed;
arbitrary CLI argument passthrough is not supported. Solver stdin is
closed, so there is no interactive input channel or multi-shot
controller interface.

The canonical representation and expansion hook support the documented
ASP syntax, not every native solver extension. Conditional bodies accept
only simple literals, not nested conditional literals or aggregates.
Constants do not accept variables or anonymous terms. Native syntax
outside this representation requires ``file/1``, ``codes/1``, or
``raw/1``.

Source inspection does not ground a program, resolve includes, or change
directories. Generated source has no implicit relative-include base. The
``program/2`` predicate does not return a source map; use
``asp_program::serialize/4`` when one is required. Raw ASP is trusted
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

Zetesis runtime verification covers 0.7.0 with the CPU backend on macOS,
not its GPU backends or Windows execution. No complete equivalence of
solver languages, grounding strategies, search histories, or performance
is claimed.
