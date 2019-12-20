..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _debugging_debugging:

Debugging
=========

The Logtalk distribution includes a command-line
`debugger <https://logtalk.org/tools.html#debugging>`_ tool
implemented as a Logtalk application. It can be loaded by typing:

.. code-block:: text

   | ?- logtalk_load(debugger(loader)).

It can also be loaded automatically at startup time by using a
:term:`settings file`. This tool implements debugging features similar to
those found on most Prolog systems. There are some differences, however,
between the usual implementation of Prolog debuggers and the current
implementation of the Logtalk debugger that you should be aware. First,
unlike most Prolog debuggers, the Logtalk debugger is not a built-in feature
but a regular Logtalk application using documented debugging hook predicates.
This translates to a different, although similar, set of debugging features
when compared with some of the more sophisticated Prolog debuggers. Second,
debugging is only possible for entities compiled in debug mode. When
compiling an entity in debug mode, Logtalk decorates clauses with source
information to allow tracing of the goal execution. Third, implementation
of spy points allows the user to specify the execution context for entering
the debugger. This feature is a consequence of the encapsulation of
predicates inside objects.

.. _debugging_debug_mode:

Compiling source files in debug mode
------------------------------------

Compilation of source files in debug mode is controlled by the
:ref:`debug <flag_debug>` compiler flag. The default value for this flag,
usually ``off``, is defined in the adapter files. Its default value may
be changed at runtime by calling:

.. code-block:: text

   | ?- set_logtalk_flag(debug, on).

In alternative, if we want to compile only some source files in debug
mode, we may instead write:

.. code-block:: text

   | ?- logtalk_load([file1, file2, ...], [debug(on)]).

The :ref:`predicates_logtalk_make_1` built-in predicate can also be used to
recompile all loaded files (that were compiled without using explicit values
for the :ref:`debug <flag_debug>` and :ref:`optimize <flag_optimize>` compiler
flags in a ``logtalk_load/2`` call or in a loader file, if used) in debug mode:

.. code-block:: text

   | ?- logtalk_make(debug).

With most :term:`backend Prolog compilers <backend Prolog compiler>`, the
``{+d}`` top-level shortcut can also be used. After debugging, the files can
be recompiled in normal or optimized mode using, respectively, the ``{+n}``
or ``{+o}`` top-level shortcuts.

The :ref:`clean <flag_clean>` compiler flag should be turned on whenever
the :ref:`debug <flag_debug>` flag is turned on at runtime. This is necessary
because debug code would not be generated for files previously compiled in
normal mode if there are no changes to the source files.

After loading the debugger, we may check (or enumerate by backtracking),
all loaded entities compiled in debug mode as follows:

.. code-block:: text

   | ?- debugger::debugging(Entity).

To compile only a specific entity in debug mode, use the
:ref:`directives_set_logtalk_flag_2` directive inside the entity.

.. _debugging_box_model:

Procedure box model
-------------------

Logtalk uses a *procedure box model* similar to those found on most
Prolog compilers. The traditional Prolog procedure box model defines
four ports (*call*, *exit*, *redo*, and *fail*) for describing control
flow when a predicate clause is used during program execution:

| ``call``
|    predicate call
| ``exit``
|    success of a predicate call
| ``redo``
|    backtracking into a predicate
| ``fail``
|    failure of a predicate call

Logtalk, as found on some recent Prolog compilers, adds a port for
dealing with exceptions thrown when calling a predicate:

| ``exception``
|    predicate call throws an exception

In addition to the ports described above, Logtalk adds two more ports,
``fact`` and ``rule``, which show the result of the unification of a
goal with, respectively, a fact and a rule head:

| ``fact``
|    unification success between a goal and a fact
| ``rule``
|    unification success between a goal and a rule head

Following Prolog tradition, the user may define for which ports the
debugger should pause for user interaction by specifying a list of
*leashed* ports. For example:

.. code-block:: text

   | ?- debugger::leash([call, exit, fail]).

Alternatively, the user may use an atom abbreviation for a pre-defined
set of ports. For example:

.. code-block:: text

   | ?- debugger::leash(loose).

The abbreviations defined in Logtalk are similar to those defined on
some Prolog compilers:

| ``none``
|    ``[]``
| ``loose``
|    ``[fact, rule, call]``
| ``half``
|    ``[fact, rule, call, redo]``
| ``tight``
|    ``[fact, rule, call, redo, fail, exception]``
| ``full``
|    ``[fact, rule, call, exit, redo, fail, exception]``

By default, the debugger pauses at every port for user interaction.

Defining spy points
-------------------

Logtalk spy points can be defined by simply stating which file line
numbers or predicates should be spied, as in most Prolog debuggers, or
by fully specifying the context for activating a spy point. In the case
of line number spy points (also known as breakpoints), the line number
must correspond to the first line of an entity clause. To simplify the
definition of line number spy points, these are specified using the
entity identifier instead of the file name (as all entities share a
single namespace, an entity can only be defined in a single file).

Defining line number and predicate spy points
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Line number and predicate spy points are specified using the debugger
``spy/1`` predicate. The argument can be a breakpoint (expressed as a
``Entity-Line`` pair), a predicate indicator (``Name/Arity``), or a
list of spy points. For example:

.. code-block:: text

   | ?- debugger::spy(person-42).

   Spy points set.
   yes

   | ?- debugger::spy(foo/2).

   Spy points set.
   yes

   | ?- debugger::spy([foo/4, bar/1]).

   Spy points set.
   yes

Line numbers and predicate spy points can be removed by using the
debugger ``nospy/1`` predicate. The argument can be a spy point, a
list of spy points, or a non-instantiated variable in which case all
spy points will be removed. For example:

.. code-block:: text

   | ?- debugger::nospy(_).

   All matching predicate spy points removed.
   yes

Defining context spy points
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A context spy point is a tuple describing a message execution context and
a goal:

::

   (Sender, This, Self, Goal)

The debugger is evoked whenever the spy point goal and the specified
execution context subsumes the goal currently being executed and its
execution context. The user may establish any number of context spy points
as necessary. For example, in order to call the debugger whenever a
predicate defined on an object named ``foo`` is called we may define
the following spy point:

.. code-block:: text

   | ?- debugger::spy(_, foo, _, _).

   Spy point set.
   yes

For example, we can spy all calls to a ``foo/2`` predicate with a `bar`
atom in the second argument by setting the condition:

.. code-block:: text

   | ?- debugger::spy(_, _, _, foo(_, bar)).

   Spy point set.
   yes

The debugger ``nospy/4`` predicate may be used to remove all matching
spy points. For example, the call:

.. code-block:: text

   | ?- debugger::nospy(_, _, foo, _).

   All matching context spy points removed.
   yes

will remove all context spy points where the value of :term:`self` matches the
atom ``foo``.

Removing all spy points
~~~~~~~~~~~~~~~~~~~~~~~

We may remove all line number, predicate, and context spy points by
using the debugger ``nospyall/0`` predicate:

.. code-block:: text

   | ?- debugger::nospyall.

   All line number spy points removed.
   All predicate spy points removed.
   All context spy points removed.
   yes

.. _programming_trace:

Tracing program execution
-------------------------

Logtalk allows tracing of execution for all objects compiled in debug
mode. To start the debugger in trace mode, write:

.. code-block:: text

   | ?- debugger::trace.

   yes

Next, type the query to be debugged. For examples, using the ``family``
example in the Logtalk distribution compiled for debugging:

.. code-block:: text

   | ?- addams::sister(Sister, Sibling).
        Call: (1) sister(_1082,_1104) ? 
        Rule: (1) sister(_1082,_1104) ? 
        Call: (2) ::female(_1082) ? 
        Call: (3) female(_1082) ? 
        Fact: (3) female(morticia) ? 
       *Exit: (3) female(morticia) ? 
       *Exit: (2) ::female(morticia) ? 
       ...

While tracing, the debugger will pause for user input at each leashed port,
printing an informative message. Each trace line starts with the port,
followed by the goal invocation number, followed by the goal. The invocation
numbers are unique and allows us to correlate the ports used for a goal.
In the output above, you can see for example that the goal ``::female(_1082)``
succeeds with the answer ``::female(morticia)``. The debugger also provides
determinism information by prefixing the ``exit`` port with a ``*`` character
when a call succeeds with choice-points pending, thus indicating that there
might be alternative solutions for the goal.

Note that, when tracing, spy points will be ignored. Before the
port number, when a spy point is set for the current clause or goal, the
debugger will print a ``#`` character for line number spy points, a
``+`` character for predicate spy points, and a ``*`` character for
context spy points. For example:

.. code-block:: text

   | ?- debugger::spy(female/2).

   yes

   | ?- addams::sister(Sister, Sibling).
        Call: (1) sister(_1078,_1100) ? 
        Rule: (1) sister(_1078,_1100) ? 
        Call: (2) ::female(_1078) ? 
     +  Call: (3) female(_1078) ? 

To stop tracing and turning off the debugger, write:

.. code-block:: text

   | ?- debugger::notrace.

   yes

.. _debugging_debug:

Debugging using spy points
--------------------------

Tracing a program execution may generate large amounts of debugging
data. Debugging using spy points allows the user to concentrate in
specific points of the code. To start a debugging session using spy
points, write:

.. code-block:: text

   | ?- debugger::debug.

   yes

For example, assuming the spy point we set in the previous section on
the ``female/1`` predicate:

.. code-block:: text

   | ?- addams::sister(Sister, Sibling).
     +  Call: (3) female(_1078) ? 

To stop the debugger, write:

::

   | ?- debugger::nodebug.

   yes

Note that stopping the debugger does not remove any defined spy points.

.. _debugging_commands:

Debugging commands
------------------

The debugger pauses at leashed ports when tracing or when finding a spy
point for user interaction. The commands available are as follows:

``c`` — creep
   go on; you may use the spacebar, return, or enter keys in alternative
``l`` — leap
   continues execution until the next spy point is found
``s`` — skip
   skips debugging for the current goal; valid at call, redo, and
   unification ports
``q`` — quasi-skip
   skips debugging until returning to the current goal or reaching a spy
   point; valid at call and redo ports
``r`` — retry
   retries the current goal but side-effects are not undone; valid at
   the fail port
``j`` — jump
   reads invocation number and continues execution until a port is
   reached for that number
``z`` — zap
   reads port name and continues execution until that port is reached
   reads negated port name and continues execution until a port other
   than the negated port is reached
``i`` — ignore
   ignores goal, assumes that it succeeded; valid at call and redo ports
``f`` — fail
   forces backtracking; may also be used to convert an exception into a
   failure
``n`` — nodebug
   turns off debugging
``@`` — command; ``!`` can be used in alternative
   reads and executes a query
``b`` — break
   suspends execution and starts new interpreter; type ``end_of_file``
   to terminate
``a`` — abort
   returns to top level interpreter
``Q`` — quit
   quits Logtalk
``p`` — print
   writes current goal using the print/1 predicate if available
``d`` — display
   writes current goal without using operator notation
``w`` — write
   writes current goal quoting atoms if necessary
``$`` — dollar
   outputs the compiled form of the current goal (for low-level
   debugging)
``x`` — context
   prints execution context
``.`` — file
   prints file, entity, predicate, and line number information at an
   unification port
``e`` — exception
   prints exception term thrown by the current goal
``=`` — debugging
   prints debugging information
``<`` — write depth
   sets the write term depth (set to 0 to reset)
``*`` — add
   adds a context spy point for the current goal
``/`` — remove
   removes a context spy point for the current goal
``+`` — add
   adds a predicate spy point for the current goal
``-`` — remove
   removes a predicate spy point for the current goal
``#`` — add
   adds a line number spy point for the current clause
``|`` — remove
   removes a line number spy point for the current clause
``h`` — condensed help
   prints list of command options
``?`` — extended help
   prints list of command options

.. _debugging_context:

Context-switching calls
-----------------------

Logtalk provides a control construct, :ref:`control_context_switch_2`,
which allows the execution of a query within the context of an object.
Common debugging uses include checking an object local predicates (e.g.
predicates representing internal dynamic state) and sending a message
from within an object. This control construct may also be used to write
unit tests.

Consider the following toy example:

::

   :- object(broken).

       :- public(a/1).

       a(A) :- b(A, B), c(B).
       b(1, 2). b(2, 4). b(3, 6).
       c(3).

   :- end_object.

Something is wrong when we try the object public predicate, ``a/1``:

.. code-block:: text

   | ?- broken::a(A).

   no

For helping diagnosing the problem, instead of compiling the object in
debug mode and doing a *trace* of the query to check the clauses for the
non-public predicates, we can instead simply type:

.. code-block:: text

   | ?- broken << c(C).

   C = 3
   yes

The ``<</2`` control construct works by switching the execution context
to the object in the first argument and then compiling and executing the
second argument within that context:

.. code-block:: text

   | ?- broken << (self(Self), sender(Sender), this(This)).

   Self = broken
   Sender = broken
   This = broken

   yes

As exemplified above, the ``<</2`` control construct allows you to call
an object local and private predicates. However, it is important to
stress that we are not bypassing or defeating an object predicate scope
directives. The calls take place within the context of the specified
object, not within the context of the object making the ``<</2`` call.
Thus, the ``<</2`` control construct implements a form of
*execution-context switching*.

The availability of the ``<</2`` control construct is controlled by the
:ref:`context_switching_calls <flag_context_switching_calls>` compiler
flag (its default value is defined in the adapter files of the backend
Prolog compilers).

.. _debugging_messages:

Debugging messages
------------------

Calls to the :ref:`logtalk::print_message/3 <methods_print_message_3>`
predicate where the message kind is either ``debug`` or ``debug(Group)`` are
only printed, by default, when the :ref:`debug <flag_debug>` flag is turned
on. Moreover, these calls are suppressed by the compiler when the
:ref:`optimize <flag_optimize>` flag is turned on. Note that actual printing
of debug messages does not require compiling the code in debug mode, only
turning on the ``debug`` flag.

Meta-messages
~~~~~~~~~~~~~

To avoid having to define :ref:`methods_message_tokens_2` grammar rules
for translating each and every debug message, Logtalk provides default
tokenization for four *meta-messages* that cover the most common cases:

``@Message``
   By default, the message is printed as passed to the ``write/1``
   predicate followed by a newline.
``Key-Value``
   By default, the message is printed as ``Key: Value`` followed by a
   newline. The value is printed as passed to the ``writeq/1``
   predicate.
``List``
   By default, the list items are printed indented one per line. The
   items are preceded by a dash and printed as passed to the
   ``writeq/1`` predicate.
``Title::List``
   By default, the title is printed followed by a newline and the
   indented list items, one per line. The items are preceded by a dash
   and printed as passed to the ``writeq/1`` predicate.

These print messages goals can always be combined with hooks as
described in the previous section to remove them in production ready
code. Some simple examples of using these meta-messages:

.. code-block:: text

   | ?- logtalk::print_message(debug, core, @'Phase 1 completed').
   yes

   | ?- set_logtalk_flag(debug, on).
   yes

   | ?- logtalk::print_message(debug, core, @'Phase 1 completed').
   >>> Phase 1 completed
   yes

   | ?- logtalk::print_message(debug, core, answer-42).
   >>> answer: 42
   yes

   | ?- logtalk::print_message(debug, core, [arthur,ford,marvin]).
   >>> - arthur
   >>> - ford
   >>> - marvin
   yes

   | ?- logtalk::print_message(debug, core, names::[arthur,ford,marvin]).
   >>> names:
   >>> - arthur
   >>> - ford
   >>> - marvin
   yes

The ``>>>`` prefix is the default message prefix for ``debug`` messages.
It can be redefined using the
:ref:`logtalk::message_prefix_stream/4 <methods_message_prefix_stream_4>`
hook predicate. For example:

::

   :- multifile(logtalk::message_prefix_stream/4).
   :- dynamic(logtalk::message_prefix_stream/4).

   logtalk::message_prefix_stream(debug, core, '(dbg) ', user_error).

Selective printing of debug messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, all debug messages are either printed or skipped, depending on
the :ref:`debug <flag_debug>` and :ref:`optimize <flag_optimize>` flags.
When the code is not compiled in optimal mode, the
`debug_messages <https://logtalk.org/tools.html#debugging>`_ tool allows
selectively enabling of debug messages per :term:`component` and per debug
group. For example, to enable all ``debug`` and ``debug(Group)`` messages
for the ``parser`` component:

.. code-block:: text

   % upon loading the tool, all messages are disabled by default: 
   | ?- logtalk_load(debug_messages(loader)).
   ...

   % enable both debug and debug(_) messages:
   | ?- debug_messages::enable(parser).
   yes

To enable only ``debug(tokenization)`` messages for the ``parser`` component:

.. code-block:: text

   % first disable any and all enabled messages:
   | ?- debug_messages::disable(parser).
   yes

   % enable only debug(tokenization) messages:
   | ?- debug_messages::enable(parser, tokenization).
   yes

See the tool documentation for more details. 

.. _debugging_hooks:

Using the term-expansion mechanism for debugging
------------------------------------------------

Debugging messages only output information by default. These messages can,
however, be intercepted to perform other actions. An alternative is to use
instead the :ref:`term-expansion mechanism <expansion_expansion>` for
conditional compilation of debugging goals. For example, assuming a
``debug/1`` predicate is used to wrap debug goals, we can define a hook
object containing the following definition for ``goal_expansion/2``:

::

   goal_expansion(debug(Goal), Goal).

When not debugging, we can use a second hook object to discard the
``debug/1`` calls by defining the predicate ``goal_expansion/2`` as
follows:

::

   goal_expansion(debug(_), true).

The Logtalk compiler automatically removes any redundant calls to the
built-in predicate ``true/0`` when compiling entity predicates.

.. _debugging_ports_profiling:

Ports profiling
---------------

The Logtalk distribution includes a
`ports_profiler <https://logtalk.org/tools.html#ports-profiler>`_ tool
based on the same procedure box model described above. This tool is
specially useful for debugging performance issues (e.g. due to lack of
determinism or unexpected backtracking). See the tool documentation for
details. 

.. _debugging_events:

Debug and trace events
----------------------

The debugging API defines two multifile predicates,
:ref:`logtalk::trace_event/2 <apis:logtalk/0::trace_event/2>` and
:ref:`logtalk::debug_handler/2 <apis:logtalk/0::debug_handler/2>` for handiling
trace and debug events. It also provides a
:ref:`logtalk::debug_handler_provider/1 <apis:logtalk/0::debug_handler_provider/1>`
multifile predicate that allows an object (or a category) to declare itself
as a debug handler provider. The Logtalk ``debugger`` and  ``ports_profiler``
tools are regular applications thar are implemented using this API, which
can also be used to implement alternative or new debugging related tools.
See the API documentation for details and the source code of the ``debugger``
and  ``ports_profiler`` tools for usage examples.
