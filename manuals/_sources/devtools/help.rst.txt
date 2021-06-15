``help``
========

This tool provides basic on-line help for Logtalk features and libraries
when running in a limited set of operating-systems. For help on the
Logtalk compiler error and warning messages, see the ``tutor`` tool.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#help <../../docs/library_index.html#help>`__

For sample queries, please see the ``SCRIPT.txt`` file in the tool
directory.

Loading
-------

::

   | ?- logtalk_load(help(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(help(tester)).

Supported operating-systems
---------------------------

Currently, support is limited to Linux, macOS, and Windows.

On Windows, the ``start`` command must be available. On Linux, the
``xdg-open`` command must be available. On macOS, the command ``open``
is used.

This tool relies on the library portable operating-system access
abstraction.

Usage
-----

After loading the tool, use the query ``help::help`` to get started.

Known issues
------------

On macOS, the ``open`` command used to open documentation URLs drops the
anchors, thus preventing navigating to the specified position on the
documentation page.

ECLiPSe defines a ``help`` prefix operator that forces wrapping this
atom between parenthesis when sending messages to the tool. E.g. use
``(help)::help`` instead of ``help::help``.
