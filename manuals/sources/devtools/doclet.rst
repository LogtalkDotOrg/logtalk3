.. _doclet:

``doclet``
==========

This folder provides a simple tool for (re)generating documentation for
a project. The tool defines a ``doclet`` object that is expected to be
extended by the user to specify a sequence of goals and a sequence of
shell commands that load the application and (re)generate its
documentation.

Doclet source files are preferably named ``doclet.lgt`` (or
``doclet.logtalk``) and the doclet object are usually named after the
application or library to be documented with a ``_doclet`` suffix. By
using an ``initialization/1`` directive to automatically send the
``update/0`` message that generates the documentation upon doclet
loading, we can abstract the name of the doclet object. The usual query
to load and run a doclet is therefore:

::

   | ?- logtalk_load([doclet(loader), doclet]).

For usage examples see the ``sample_doclet.lgt``, ``doclet1.lgt``,
``zoom_doclet.lgt``, and ``tools_doclet.lgt`` source files.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#doclet <../../docs/library_index.html#doclet>`__

For sample queries, please see the ``SCRIPT.txt`` file in the tool
directory.

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(doclet(loader)).

Automating running doclets
--------------------------

You can use the ``scripts/logtalk_doclet.sh`` Bash shell script for
automating running doclets. The script expects the doclet source files
to be named either ``doclet.lgt`` or ``doclet.logtalk``. See the
``scripts/NOTES.md`` file or the script man page for details.

Integration with the make tool
------------------------------

Loading this tool adds a definition for the
``logtalk_make_target_action/1`` hook predicate for the target
``documentation``. The hook definition sends an ``update/0`` message to
each loaded doclet.
