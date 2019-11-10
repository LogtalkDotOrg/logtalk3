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

`docs/library_index.html#debug-messages <https://logtalk.org/docs/library_index.html#debug-messages>`__

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
``debug(Group)`` messages for a given group and component for
fine-grained control.

Upon loading the tool, all debug messages are skipped. The user is then
expected to use the tool API to selectively enable the messages that
will be printed. As an example, consider the following object, part of a
``xyz`` component:

::

   :- object(foo).

       :- public([bar/0, baz/0]).
       :- uses(logtalk, [print_message/3]).

       bar :-
           print_message(debug(bar), xyz, @'bar/0 called').

       baz :-
           print_message(debug(baz), xyz, @'baz/0 called').

   :- end_object.

Assuming the object ``foo`` is compiled and loaded in normal or debug
mode, after also loading this tool, ``bar/0`` and ``baz/0`` messages
will not print any debug messages:

::

   ?- {debug_messages(loader), foo}.
   ...
   yes

   ?- foo::(bar, baz).
   yes

We can then enable all debug messages for the ``xyz`` component:

::

   ?- debug_messages::enable(xyx).
   yes

   ?- foo::(bar, baz).
   bar/0 called
   baz/0 called
   yes

Or we can selectively enable only debug messages for a specific group:

::

   ?- debug_messages::disable(xyx).
   yes

   ?- debug_messages::enable(xyx, bar).
   yes

   ?- foo::(bar, baz).
   bar/0 called
   yes

Other notes
-----------

All source files are indented using tabs (a common setting is a tab
width equivalent to 4 spaces).
