.. _library_linter_reporter:

``linter_reporter``
===================

This tool intercepts compiler linter warnings and caches them to
generate SARIF reports that can be used in CI/CD pipelines.

API documentation
-----------------

This tool API documentation is available at:

`../../apis/library_index.html#linter_reporter <../../apis/library_index.html#linter_reporter>`__

Loading
-------

Load the tool before loading the code to be checked:

::

   | ?- logtalk_load(linter_reporter(loader)).
   ...

Enable collecting linter warnings data using default options:

::

   | ?- linter_reporter::enable.
   true.

Or using explicit options:

::

   | ?- linter_reporter::enable([explanations(true)]).
   true.

Load the code for which you want a SARIF report generated:

::

   | ?- logtalk_load(my_application(loader)).
   ...

Disable further collecting of linter warnings:

::

   | ?- linter_reporter::disable.
   true.

Save the SARIF report:

::

   | ?- linter_reporter::report.

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(linter_reporter(tester)).

The test suite reuses ``errors`` example files to exercise
representative built-in linter warnings and validates SARIF export in
both explanation-disabled and explanation-enabled configurations.

Usage
-----

Load the tool, call ``enable/0-1`` before compiling the code to be
checked, call ``disable/0`` when warning collection is finished, and
then call ``report/0-1`` to generate the SARIF report from the cached
warnings. The ``report/0`` predicate writes the SARIF report to
``./linter_warnings.sarif``.

Options
-------

- ``explanations(Boolean)`` Boolean option accepted by ``enable/1``.
  When set to ``true`` (default is ``false``), the tool enriches
  warnings with explanations from the ``tutor_explanations`` category
  provided by the ``tutor`` tool.
