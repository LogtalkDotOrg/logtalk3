.. _tutor:

``tutor``
=========

This tool adds explanations and suggestions to selected compiler warning
and error messages. It's most useful for new users not yet familiar with
the compiler and runtime warning and error messages.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#tutor <../../docs/library_index.html#tutor>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(tutor(loader)).

Usage
-----

Simply load the tool at startup (e.g. from a settings file).
