Overview
========

The following developer tools are available, each one with its own
``loader.lgt`` loader file (except for the built-in ``linter`` and
``make`` tools, which are integrated with the compiler/runtime) and
``NOTES.md`` documentation files:

-  ``asdf``
-  ``assertions``
-  ``code_metrics``
-  ``dead_code_scanner``
-  ``debug_messages``
-  ``debugger``
-  ``diagrams``
-  ``doclet``
-  ``help``
-  ``issue_creator``
-  ``lgtdoc``
-  ``lgtunit``
-  ``linter``
-  ``make``
-  ``packs``
-  ``ports_profiler``
-  ``profiler``
-  ``tutor``
-  ``wrapper``

Loading the developer tools
---------------------------

To load the main developer tools, use the following goal:

::

   | ?- logtalk_load(tools(loader)).

The ``ports_profiler`` tool is not loaded by default, however, as it
conflicts with the ``debugger`` tool as both provide a debug handler
that must be unique in a running session.

The ``profiler`` tool is also not loaded by default as it provides
integration with selected backend Prolog compiler profilers that are not
portable.

The ``tutor`` tool is also not loaded by default given its useful mainly
for new users that need help understanding compiler warning and error
messages.

The ``wrapper`` tool is also not loaded by default given its specialized
purpose and beta status.

To load a specific tool, either change your Prolog working directory to
the tool folder and then compile and load the corresponding loader
utility file or simply use library notation as argument for the
compiling and loading predicates. For example:

::

   | ?- logtalk_load(lgtunit(loader)).

Tools documentation
-------------------

Specific notes about each tool can be found in the corresponding
``NOTES.md`` files. HTML documentation for each tool API can be found on
the ``docs`` directory (open the ``../docs/index.html`` file with your
web browser). The documentation for these tools can be regenerated using
the shell scripts ``../scripts/update_html_docs.sh`` and
``../scripts/update_svg_diagrams.sh``.
