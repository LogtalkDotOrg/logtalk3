``debug_messages``
==================

By default, ``debug`` and ``debug(Group)`` messages are only printed
when the ``debug`` flag is turned on. These messages are also suppressed
when compiling code with the ``optimize`` flag turned on. This tool
supports selective enabling of ``debug`` and ``debug(Group)`` messages
in normal and debug modes.

API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

`docs/library_index.html#debug_messages <https://logtalk.org/docs/library_index.html#debug_messages>`__

For general information on debugging, open in a web browser the
following file and consult the debugging section of the User Manual:

`manuals/userman/debugging.html <https://logtalk.org/manuals/userman/debugging.html>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(debug_messages(loader)).

Usage
-----

The tool provides two sets of predicates. The first set allows enabling
and disabling of all ``debug`` and ``debug(Group)`` messages for a given
component. The second set allows enabling and disabling of
``debug(Group)`` messages for a given group and component.

Upon loading the tool, all debug messages are skipped. The user is then
expected to use the tool API to selectively enable the messages that
will be printed.

Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
