.. _library_sarif:

``sarif``
=========

The ``sarif`` tool serializes diagnostics produced by tools implementing
the ``tool_diagnostics_protocol`` protocol into SARIF 2.1.0 reports:

https://sarifweb.azurewebsites.net/

API documentation
-----------------

This tool API documentation is available at:

`../../apis/library_index.html#sarif <../../apis/library_index.html#sarif>`__

Loading
-------

Load the tool using:

::

   | ?- logtalk_load(sarif(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(sarif(tester)).

The test suite validates SARIF generation for single diagnostics
producers and explicit aggregate reports, including JSON Schema
validation against the SARIF 2.1.0 schema.

Usage
-----

Use the ``term/4`` and ``generate/4`` predicates to generate a report
for a single diagnostics producer, target, and options combination.
These predicates generate a SARIF document with a single run.

For example:

::

   | ?- sarif::generate(dead_code_scanner, entity(my_object), file('./report.sarif'), []).
   true.

Use the ``term/2`` and ``generate/2`` predicates to generate an explicit
aggregate report from a list of specifications. Each specification must
be a ``tool_spec(Tool, Target, Options)`` term and produces a single
SARIF run. Runs are emitted in the same order as the specifications
list.

For example:

::

   | ?- sarif::generate([
            tool_spec(linter_reporter, all, []),
            tool_spec(dead_code_scanner, entity(my_object), [])
        ], file('./aggregate.sarif')).
   true.

Each specification uses the same target and options accepted by the
corresponding diagnostics producer. The aggregate API is explicit: the
``sarif`` tool does not infer producers or merge options across
specifications.

Diagnostics producers
---------------------

The following tools produce diagnostics compatible with the ``sarif``
tool:

- ``dead_code_scanner``
- ``lgtdoc``
- ``lgtunit``
- ``linter_reporter``

See the individual tools documentation for details on how to generate
diagnostics.

Report visualization
--------------------

SARIF reports can be visualized using e.g. VSCode extensions or online
viewers. SARIF reports are also supported in e.g. GitHub, GitLab, and
BitBucket.
