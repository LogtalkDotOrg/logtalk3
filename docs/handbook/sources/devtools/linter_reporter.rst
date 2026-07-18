.. _library_linter_reporter:

``linter_reporter``
===================

This tool intercepts compiler linter warnings and caches them as
machine-readable diagnostics. These diagnostics can be queried directly
or serialized as a SARIF report using the standalone ``sarif`` tool.

API documentation
-----------------

This tool API documentation is available at:

`../../apis/library_index.html#linter_reporter <../../apis/library_index.html#linter_reporter>`__

Loading
-------

Load the tool using:

::

   | ?- logtalk_load(linter_reporter(loader)).
   ...

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(linter_reporter(tester)).

The test suite reuses ``errors`` example files to exercise
representative built-in linter warnings and validates both the
diagnostics API and standalone SARIF generation in explanation-disabled
and explanation-enabled configurations.

Usage
-----

After loading the tool, enable collecting linter warnings data using
default options:

::

   | ?- linter_reporter::enable.
   true.

Or using explicit options:

::

   | ?- linter_reporter::enable([explanations(true)]).
   true.

Load the code for which you want diagnostics collected:

::

   | ?- logtalk_load(my_application(loader)).
   ...

Disable further collecting of linter warnings:

::

   | ?- linter_reporter::disable.
   true.

Query the cached diagnostics directly:

::

   | ?- linter_reporter::diagnostics(all, Diagnostics).
   ...

Warnings originating in an included file are not always file-scoped.
When the ``include/1`` directive appears inside an entity, the
diagnostic context can be that entity; otherwise the context is
typically the included file.

Or generate a SARIF report using the standalone ``sarif`` tool:

::

   | ?- logtalk_load(sarif(loader)).
   ...

   | ?- sarif::generate(linter_reporter, all, file('./linter_warnings.sarif'), []).
   true.

See the ``sarif`` tool documentation for more details.

Options
-------

- ``explanations(Boolean)`` Boolean option accepted by ``enable/1``.
  When set to ``true`` (default is ``false``), the tool enriches
  warnings with explanations from the ``tutor_explanations`` category
  provided by the ``tutor`` tool.
