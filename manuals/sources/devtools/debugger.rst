``debugger``
============

This tool provides the default Logtalk command-line debugger.

API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

`docs/library_index.html#debugger <https://logtalk.org/docs/library_index.html#debugger>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(debugger(loader)).

Note that this tool cannot be loaded at the same time as other tools
(e.g. the ports profiler) that also provide a debug handler, which must
be unique in a running session.

When the code to be debugged runs computationally expensive
initializations, loading this tool after the code may have a noticeable
impact in loading times.

Usage
-----

Debugging Logtalk source code (with this debugger) requires compiling
source files using the ``debug(on)`` compiler flag. For example:

::

   | ?- logtalk_load(my_buggy_code, [debug(on)]).

In alternative, you may also turn on the ``debug`` flag globally by
typing:

::

   | ?- set_logtalk_flag(debug, on).

But note that loader files may override this flag setting (e.g. by using
``debug(off)`` or ``optimize(on)`` options for loaded files). If that's
the case, you will need to either edit the loader files or write
customized loader files enabling debugging. For detailed information on
using the debugger, consult the debugging section of the User Manual:

`manuals/userman/debugging.html <https://logtalk.org/manuals/userman/debugging.html>`__

Logtalk also provides basic support for the SWI-Prolog graphical tracer.
The required settings are described in the ``settings-sample.lgt`` file.

The ``debugger_messages.lgt`` source file defines the default debugger
message translations.

The ``dump_trace.lgt`` contains a simple code example on how to redirect
a goal trace to a file.

Known issues
------------

Line number spy points require a Prolog backend compiler that supports
accessing read term starting line but only some systems (B-Prolog,
JIProlog, Lean Prolog, SWI-Prolog, and YAP) provide accurate line
numbers.

As a workaround, you can check the start line number for an entity
predicate definition using a query such as:

::

   | ?- object_property(Entity, defines(Functor/Arity, Properties)).

and checking the returned ``line_count/1`` property to find if there's
any offset to the source file number of the predicate clause that you
want to trace. This issue, if present, usually only affects the first
predicate clause.

Line number spy points are currently not available when using XSB as the
Prolog backend compiler.

Other notes
-----------

All source files are indented using tabs (a common setting is a tab
width equivalent to 4 spaces).
