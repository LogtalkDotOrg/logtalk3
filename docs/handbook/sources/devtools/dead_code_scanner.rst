.. _library_dead_code_scanner:

``dead_code_scanner``
=====================

This tool detects *likely* dead code in Logtalk entities and in Prolog
modules compiled as objects. Predicates (and non-terminals) are
classified as dead code when:

- There is no scope directive for them, and they are not called,
  directly or indirectly, by any predicate with a (local or inherited)
  scope directive.
- They are listed in ``uses/2`` and ``use_module/2`` directives but not
  called.

Besides dead code, this tool can also help detect other problems in the
code that often result in reporting false positives. For example, typos
in ``alias/2`` directives, missing scope directives, and missing
``meta_non_terminal/1`` and ``meta_predicate/1`` directives.

Given the possibility of false positives, care must be taken before
deleting reported dead code to ensure that it's, in fact, code that is
not used. A common cause of false positives is the use of conditional
compilation directives to provide implementations for predicates missing
in some systems or different predicate implementations per
operating-system.

The ``dead_code_scanner.lgt`` source file implements the scanning
predicates for finding dead code in entities, libraries, and
directories. The source file ``dead_code_scanner_messages.lgt`` defines
the default translations for the messages printed when scanning for dead
code. These messages can be intercepted to customize the output, e.g.,
to make it less verbose or for integration with, e.g., GUI IDEs and
continuous integration servers.

API documentation
-----------------

This tool API documentation is available at:

`../../apis/library_index.html#dead-code-scanner <../../apis/library_index.html#dead-code-scanner>`__

For sample queries, please see the ``SCRIPT.txt`` file in the tool
directory.

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(dead_code_scanner(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(dead_code_scanner(tester)).

Usage
-----

This tool provides a set of predicates that allows scanning entities,
libraries, files, and directories. See the tool API documentation for
details. The source code to be analyzed should be loaded with the
``source_data`` and ``optimize`` flags turned on (possibly set from a
loader file).

For machine-readable integrations, the ``findings/2-3``,
``finding/2-3``, and ``summary/2-3`` predicates can be used to collect
dead-code results and post- filter counts without relying on printed
messages. Findings are returned as terms of the form:

- ``dead_predicate(Class, Confidence, Properties, EntityKind, Entity, Predicate, File, Lines)``

where:

- ``Class`` distinguishes local dead code from unused ``uses/2`` and
  ``use_module/2`` resources
- ``Confidence`` is a triage-oriented rating (``high``, ``medium``, or
  ``low``)
- ``Properties`` contains additional structured metadata, including the
  analysis settings seen for the source file and any class-specific
  details needed to explain the finding

For machine-readable export integrations, the ``export/3-4`` predicates
serialize scan results using the ``json`` library. Supported formats are
``json`` and ``sarif``. The JSON export format is described by the
``dead_code_scanner.schema.json`` JSON Schema file in the tool
directory. Runtime validation of exported JSON against that schema can
be enabled using the ``validate_export(true)`` option. The default is
``validate_export(false)``. The current JSON export format version is
``1.0.0``.

As an example, assume that we want to scan an application with a library
alias ``my_app``. The following goals could be used:

::

   | ?- set_logtalk_flag(source_data, on),
        set_logtalk_flag(optimize, on).
   yes

   | ?- logtalk_load(my_app(loader)).
   ...
   yes

   | ?- dead_code_scanner::library(my_app).
   ...

For complex applications that make use of sub-libraries, there are also
``rlibrary/1-2`` predicates that perform a recursive scan of a library
and all its sub-libraries. Conversely, we may be interested in scanning
a single entity:

::

   | ?- dead_code_scanner::entity(some_object).
   ...

For other usage examples, see the ``SCRIPT.txt`` file in the tool
directory.

Excluding code from analysis
----------------------------

A set of options is available to specify code that should be excluded
when looking for unused predicates (and non-terminals):

- | ``exclude_directories(Directories)``
  | list of directories to exclude (default is ``[]``); all
    sub-directories of the excluded directories are also excluded;
    directories may be listed by full or relative path

- | ``exclude_files(Files)``
  | list of source files to exclude (default is ``[]``); files may be
    listed by full path or basename, with or without extension

- | ``exclude_libraries(Libraries)``
  | list of libraries to exclude (default is
    ``[startup, scratch_directory]``)

- | ``exclude_entities(Entities)``
  | list of entities to exclude (default is ``[]``)

- | ``exclude_predicates(Predicates)``
  | list of predicate and non-terminal indicators to exclude from
    findings (default is ``[]``); supports local indicators such as
    ``foo/1`` and ``bar//2`` plus qualified indicators such as
    ``object::baz/2`` and ``module:qux/3``

- | ``waive_findings(Findings)``
  | list of finding patterns to suppress from the results (default is
    ``[]``); intended for reusable CI baselines and supports partially
    instantiated structured finding terms such as
    ``dead_predicate(local_dead_code, medium, _, object, some_object, helper/2, _, _)``

- | ``validate_export(Boolean)``
  | boolean option controlling runtime validation of ``json`` exports
    against the ``dead_code_scanner.schema.json`` file (default is
    ``false``)

Machine-readable summaries
--------------------------

The ``summary/2-3`` predicates return a term of the form:

- ``summary(Target, TotalEntities, TotalFindings, Breakdown, EntitySummaries)``

where ``Breakdown`` is a term of the form:

- ``finding_breakdown(ClassCounts, ConfidenceCounts)``

with ``ClassCounts`` containing ``class_count(Class, Count)`` terms and
``ConfidenceCounts`` containing ``confidence_count(Confidence, Count)``
terms.

``EntitySummaries`` is a list of terms of the form:

- ``entity_summary(Kind, Entity, FindingsCount, Breakdown)``

The summary is computed after applying all exclusions and finding
waivers, making it suitable for CI thresholds and regression tracking.

Machine-readable preflight warnings
-----------------------------------

The ``preflight/2-3`` predicates return an ordered set of warnings
describing analysis prerequisites that affect result quality for a
target. Warning terms currently use the form:

- ``missing_analysis_prerequisite(File, Prerequisite)``

where ``Prerequisite`` is currently one of:

- ``source_data``
- ``optimize``

This API is intended for CI and editor integrations that need to surface
analysis quality issues without scraping console messages.

Finding classes and confidence
------------------------------

The current finding classes are:

- ``local_dead_code``
- ``unused_uses_resource``
- ``unused_use_module_resource``

Confidence is a triage-oriented rating attached to every finding. The
JSON schema allows the values ``high``, ``medium``, and ``low``, but the
tool currently emits only ``high`` and ``medium``:

- ``high``

  Used when the tool has stronger evidence that the reported code is
  really unused.

- ``medium``

  Used when the result is still useful, but can be affected by missing
  compilation information such as optimized call graph data.

- ``low``

  Reserved for future use. It is accepted by the export schema but is
  not currently generated by the scanner.

For ``local_dead_code``, the confidence depends on the effective
compilation mode of the analyzed file:

- If the file was loaded in normal mode, or otherwise not loaded with
  ``optimize(on)``, the confidence is ``medium``.

- If the file was loaded with ``optimize(on)``, the confidence is
  upgraded to ``high`` because the compiler can provide more complete
  call graph information, reducing the risk of false positives caused by
  missed meta-calls.

The effective mode is inferred from the loaded file itself, not from the
current scanner defaults. Therefore, results reflect how the analyzed
code was actually compiled.

Unused ``uses/2`` and ``use_module/2`` resources are always reported
with ``high`` confidence because they are derived from the generated
linking clauses and the recorded call graph rather than from heuristic
reachability alone.

When scanning using the text-based predicates such as ``entity/1-2``,
``file/1-2``, and ``all/0-1``, the tool now emits preflight warnings
whenever the analyzed files were not loaded with ``source_data(on)`` or
``optimize(on)`` so that potentially less reliable results can be
triaged more carefully.

Exports
-------

The ``export/3-4`` predicates serialize scan results in either ``json``
or ``sarif`` format to any sink supported by the ``json::generate/2``
predicate, including files, streams, atoms, chars, and codes.

The ``json`` export is described by the
``dead_code_scanner.schema.json`` file, which can be used by third-party
tools to validate and consume that export format. The ``sarif`` export
uses the SARIF ``2.1.0`` JSON format and is suitable for code scanning
integrations that consume SARIF reports. SARIF exports include a per-run
UUID GUID, stable GUIDs for the SARIF driver and rule descriptors, and
deterministic result fingerprints derived from the canonical finding.

SARIF findings are exported using one rule descriptor per finding class:

- ``local_dead_code``
- ``unused_uses_resource``
- ``unused_use_module_resource``

Result severities are also triage-aware instead of using a single static
SARIF level for all findings:

- ``local_dead_code`` with ``high`` confidence is exported as
  ``warning``
- ``local_dead_code`` with ``medium`` or ``low`` confidence is exported
  as ``note``
- ``unused_uses_resource`` with ``high`` confidence is exported as
  ``error``
- ``unused_use_module_resource`` with ``high`` confidence is exported as
  ``error``

This mapping allows CI consumers to distinguish advisory dead-code
findings from stronger unused-resource findings without reinterpreting
the custom ``class`` and ``confidence`` properties.

JSON exports include an additive top-level ``preflight`` object with a
``warnings`` array. Each warning entry records the warning kind, file,
prerequisite, and a machine-readable severity.

SARIF exports include the same preflight information as invocation-level
``toolExecutionNotifications``, allowing consumers to distinguish
prerequisite warnings from dead code findings. representation. When the
analyzed code is inside a git repository, the export also includes the
current branch and commit hash as optional run properties. When the
repository remote URI can also be derived, the export includes a
``versionControlProvenance`` entry with the repository URI, revision id,
branch, and local repository root mapping; outside a git repository, or
when the repository URI cannot be derived, these git-derived SARIF
properties are simply omitted.

Integration with the ``make`` tool
----------------------------------

The ``loader.lgt`` file sets a make target action that will call the
``dead_code_scanner::all`` goal whenever the ``logtalk_make(check)``
goal (or its top-level abbreviation, ``{?}``) is called.

Caveats
-------

Use of local meta-calls with goal arguments only known at runtime can
result in false positives. When using library or user-defined
meta-predicates, compilation of the source files with the ``optimize``
flag turned on may allow meta-calls to be resolved at compile-time and
thus allow calling information for the meta-arguments to be recorded,
avoiding false positives for predicates that are only meta-called.

Scanning Prolog modules
-----------------------

This tool can also be applied to Prolog modules that Logtalk is able to
compile as objects. For example, if the Prolog module file is named
``module.pl``, try:

::

   | ?- logtalk_load(module, [source_data(on)]).

Due to the lack of standardization of module systems and the abundance
of proprietary extensions, this solution is not expected to work for all
cases.

Scanning plain Prolog files
---------------------------

This tool can also be applied to plain Prolog code. For example, if the
Prolog file is named ``code.pl``, simply define an object including its
code:

::

   :- object(code).
       :- include('code.pl').
   :- end_object.

Save the object to an e.g. ``code.lgt`` file in the same directory as
the Prolog file and then load it in debug mode:

::

   | ?- logtalk_load(code, [source_data(on), optimize(on)]).

In alternative, use the ``object_wrapper_hook`` provided by the
``hook_objects`` library:

::

   | ?- logtalk_load(hook_objects(loader)).
   ...

   | ?- logtalk_load(code, [hook(object_wrapper_hook), source_data(on), optimize(on)]).

With either wrapping solution, pay special attention to any compilation
warnings that may signal issues that could prevent the plain Prolog from
being fully analyzed when wrapped by an object.
