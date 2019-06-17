``help``
========

This tool provides basic on-line help for Logtalk when running in a
limited set of operating-systems.

API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

`docs/library_index.html#help <https://logtalk.org/docs/library_index.html#help>`__

For sample queries, please see the `SCRIPT.txt <SCRIPT.txt>`__ file in
the tool directory.

Loading
-------

::

   | ?- logtalk_load(help(loader)).

Supported operating-systems
---------------------------

Currently, support is limited to Linux, macOS, and Windows.

On Windows, the ``start`` command must be available. On Linux, the
``xdg-open`` command must be available. On macOS, the command ``open``
is used.

This tool relies on the library portable operating-system access
abstraction.

Known issues
------------

On macOS, the ``open`` command used to open documentation URLs drops the
anchors, thus preventing navigating to the specified position on the
documentation page.

ECLiPSe defines a ``help`` prefix operator that forces wrapping this
atom between parenthesis when sending messages to the tool. E.g. use
``(help)::help`` instead of ``help::help``.

Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
