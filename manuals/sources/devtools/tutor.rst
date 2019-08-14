``tutor``
=========

This tool adds explanations and suggestions to selected compiler warning
and error messages. It's most useful for new users not yet familiar with
the compiler and runtime warning and error messages.

API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

`docs/library_index.html#tutor <https://logtalk.org/docs/library_index.html#tutor>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(tutor(loader)).

Usage
-----

Simply load the tool at startup (e.g. from a settings file).

Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
