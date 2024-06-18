..
   This file is part of Logtalk <https://logtalk.org/>  
   SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

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

The Logtalk distribution includes a command-line :doc:`../devtools/debugger`
tool implemented as a Logtalk application. It can be loaded by typing:

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
information to allow tracing of the goal execution. Third, the implementation
of spy points allows the user to specify the
:term:`execution context <predicate execution context>` for entering
the debugger. This feature is a consequence of the encapsulation of
predicates inside objects.

.. _debugging_debug_mode:

Compiling source files in debug mode
------------------------------------

Compilation of source files in debug mode is controlled by the
:ref:`debug <flag_debug>` compiler flag. The default value for this flag,
usually ``off``, is defined in the adapter files. Its default value may
be changed globally at runtime by calling:

.. code-block:: text

   | ?- set_logtalk_flag(debug, on).

Implicitly, this goal also turns off the ``optimize`` flag. In alternative,
if we want to compile only some source files in debug mode, we may instead
write:

.. code-block:: text

   | ?- logtalk_load([file1, file2, ...], [debug(on)]).

The :ref:`predicates_logtalk_make_1` built-in predicate can also be used to
recompile all loaded files (that were compiled without using explicit values
for the :ref:`debug <flag_debug>` and :ref:`optimize <flag_optimize>` compiler
flags in a ``logtalk_load/2`` call or in a :term:`loader file` file, if used)
in debug mode:

.. code-block:: text

   | ?- logtalk_make(debug).

With most :term:`backend Prolog compilers <backend Prolog compiler>`, the
``{+d}`` top-level shortcut can also be used. After debugging, the files can
be recompiled in normal or optimized mode using, respectively, the ``{+n}``
or ``{+o}`` top-level shortcuts.

.. warning::

   The :ref:`clean <flag_clean>` compiler flag should be turned on whenever
   the :ref:`debug <flag_debug>` flag is turned on at runtime. This is necessary
   because debug code would not be generated for files previously compiled in
   normal or optimized mode if there are no changes to the source files.

After loading the debugger, we may check (or enumerate by backtracking),
all loaded entities compiled in debug mode as follows:

.. code-block:: text

   | ?- debugger::debugging(Entity).

To compile only a specific entity in debug mode, use the
:ref:`directives_set_logtalk_flag_2` directive inside the entity.
To compile all entities in a source file in debug mode, use the
:ref:`directives_set_logtalk_flag_2` directive at the beginning
of the file.

.. _debugging_box_model:

Procedure box model
-------------------

Logtalk uses a *procedure box model* similar to those found on most
Prolog systems. The traditional Prolog procedure box model defines
four ports (*call*, *exit*, *redo*, and *fail*) for describing control
flow when calling a predicate:

| ``call``
|    predicate call
| ``exit``
|    success of a predicate call
| ``redo``
|    backtracking into a predicate
| ``fail``
|    failure of a predicate call

Logtalk, as found on some recent Prolog systems, adds a port for
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
*leashed* ports. Unleashed ports are just printed with no pause for
user interaction. For example:

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


Activating the debugger
-----------------------

The :ref:`debuggerp::trace/0 <apis:debuggerp/0::trace/0>` and
:ref:`debuggerp::debug/0 <apis:debuggerp/0::debug/0>` predicates implicitly
select the `debugger` tool as the active debug handler. If you have additional
debug handlers loaded (e.g. the `ports_profiler` tool), those would no longer
be active (there can be only one active debug handler at any given time). The
:ref:`debuggerp::nodebug/0 <apis:debuggerp/0::nodebug/0>` predicate implicitly
deselects the `debugger` tool as the active debug handler.


Defining spy points and breakpoints
-----------------------------------

Traditional spy points can be defined by simply stating which predicates
should be spied (as common in Prolog debuggers), by specifying the execution
context for activating a spy point, or by specifying as *breakpoints* the
heads of predicate clauses. To simplify the definition of breakpoints, these
are specified using the entity identifier instead of the file name (as all
entities share a single namespace, an entity can only be defined in a single
file) and the first line number of clause head.

Defining breakpoints and predicate spy points
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Breakpoints and predicate spy points are specified using the debugger
``spy/1`` predicate. The argument can be a breakpoint (expressed as a
``Entity-Line`` pair), a predicate indicator (``Name/Arity``), a
non-terminal indicator (``Name//Arity``), or a list of spy points. For
example:

.. code-block:: text

   | ?- debugger::spy(person-42).

   Spy points set.
   yes

   | ?- debugger::spy(foo/2).

   Spy points set.
   yes

   | ?- debugger::spy([foo/4, bar//1, agent-99]).

   Spy points set.
   yes

Note that setting a breakpoint implicitly removes any existing conditional
breakpoint, triggered breakpoint, or log point for the same location.

Unconditional breakpoints and predicate spy points can be removed by using
the debugger ``nospy/1`` predicate. The argument can also be a list or a
non-instantiated variable in which case all breakpoints and predicate spy
points will be removed. For example:

.. code-block:: text

   | ?- debugger::nospy(_).

   All matching predicate spy points removed.
   yes

In breakpoints, the line number must for the first line of a clause that
we want to spy. But note that only some Prolog backends provide accurate
source file term line numbers. Check the :doc:`../devtools/debugger` tool
documentation for details.

Defining conditional breakpoints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Conditional breakpoints are specified using the debugger ``spy/3`` predicate.
The condition can be a clause head successful unification count expression, a
lambda expression, or another breakpoint (see next section).

The supported lambda expressions are ``[Count, N, Goal]>>Condition`` and
``[Goal]>>Condition`` where ``Count`` is the unification count, ``N`` is the
goal invocation number, and ``Goal`` is the goal that unified with the clause
head; ``Condition`` is called in the context of the ``user`` pseudo-object and
must not have any side effects.

Some examples:

.. code-block:: text

   | ?- debugger::spy(planet, 76, [weight(m1,_)]>>true).

   Conditional breakpoint added.
   yes

   | ?- debugger::spy(planet, 41, =<(2)).

   Conditional breakpoint added.
   yes

The valid unification count expressions are:

- ``>(Count)`` - break when the unification count is greater than ``Count``
- ``>=(Count)`` - break when the unification count is greater than or equal to ``Count``
- ``=:=(Count)`` - break when the unification count is equal to ``Count``
- ``=<(Count)`` - break when the unification count is less than or equal to ``Count``
- ``<(Count)`` - break when the unification count is less than ``Count``
- ``mod(M)`` - break when the unification count modulo ``M`` is zero
- ``Count`` - break when the unification count is greater than or equal to ``Count``

Note that setting a conditional line number spy point will remove any existing
log point for the same location.

Conditional line numbers spy points can be removed by using the debugger
``nospy/3`` predicate. For example:

.. code-block:: text

   | ?- debugger::nospy(planet, _, _).

   All matching conditional breakpoints removed.
   yes

The line number must for the first line of a clause that we want to
conditionally spy. But note that only some Prolog backends provide
accurate source file term line numbers.
Check the :doc:`../devtools/debugger` tool documentation for details.


Defining triggered breakpoints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Conditional breakpoints that depend on other breakpoints are known as
*triggered* breakpoints. The debugger only breaks at a triggered line
number spy point if the log point or line number spy point it depends
on is hit first. For example:

.. code-block:: text

   | ?- debugger::spy(mars, 98, planet-76).

   Triggered breakpoint added.
   yes

The debugger prints a `^` character at the beginning of the line for easy
recognition of triggered breakpoints.


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

Removing all breakpoints and spy points
~~~~~~~~~~~~~~~~~~~~~~~----------------

We can remove all breakpoints and spy points by using the debugger
``nospyall/0`` predicate:

.. code-block:: text

   | ?- debugger::nospyall.

   All breakpoints removed.
   All predicate spy points removed.
   All context spy points removed.
   yes

There's also a ``reset/0`` predicate that can be used to reset the debugger
to its default settings.

Defining log points
-------------------

Logtalk log points are similar to breakpoints and thus the line
number must correspond to the first line of an entity clause. When the
debugger reaches a log point, it prints a log message and continues without
halting execution for taking a port command. When the log message is an
empty atom, the default port output message is printed. When the log message
starts with a ``%`` character, the default port output message is printed
followed by the log message. In these two cases, the debugger prints a ``@``
character at the beginning of the line for easy recognition of log points
output. When the log message is neither empty or starts with a ``%`` character,
the log message is printed instead of the default port output message. In this
case, the message can contain ``$KEYWORD`` placeholders that are expanded at
runtime. The valid keywords are:

- ``PORT``
- ``ENTITY``
- ``CLAUSE_NUMBER``
- ``FILE``
- ``LINE``
- ``UNIFICATION_COUNT``
- ``INVOCATION_NUMBER``
- ``GOAL``
- ``PREDICATE``
- ``EXECUTION_CONTEXT``
- ``SENDER``
- ``THIS``
- ``SELF``
- ``METACALL_CONTEXT``
- ``COINDUCTION_STACK``
- ``THREAD``

Log points are defined using the ``log/3`` predicate. For example:

.. code-block:: text

   | ?- debugger::log(agent, 99, '% At the secret headquarters!').
        Log point added.
   yes

   | ?- debugger::log(loop, 42, 'Message $PREDICATE from $SENDER at thread $THREAD').
        Log point added.
   yes

Predicates ``logging/3`` and ``nolog/3`` can be used to, respectively, query
and remove log points. There's also a ``nologall/0`` predicate that removes
all log points.

Note that setting a log point will remove any existing line number spy point
for the same location.

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

Note that, when tracing, spy points will be ignored. Before the port number,
when a spy point is set for the current clause or goal, the debugger will
print a ``#`` character for unconditional breakpoints, a ``?`` character for
conditional breakpoints, a ``+`` character for predicate spy
points, and a ``*`` character for context spy points. For example:

.. code-block:: text

   | ?- debugger::spy(female/2).

   yes

   | ?- addams::sister(Sister, Sibling).
        Call: (1) sister(_1078,_1100) ? 
        Rule: (1) sister(_1078,_1100) ? 
        Call: (2) ::female(_1078) ? 
     +  Call: (3) female(_1078) ? 

To stop tracing (but still allowing the debugger to stop at defined spy points),
write:

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
``S`` - Skip
   similar to skip but displaying all intermediate ports unleashed
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
   reads either a port name and continues execution until that port is
   reached or a negated port name and continues execution until a port
   other than the negated port is reached
``i`` — ignore
   ignores goal, assumes that it succeeded; valid at call and redo ports
``f`` — fail
   forces backtracking; may also be used to convert an exception into a
   failure
``n`` — nodebug
   turns off debugging
``N`` — notrace
   turns off tracing
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
   writes current goal using the ``print/1`` predicate if available
``d`` — display
   writes current goal without using operator notation
``w`` — write
   writes current goal quoting atoms if necessary
``$`` — dollar
   outputs the compiled form of the current goal (for low-level debugging)
``x`` — context
   prints execution context
``.`` — file
   prints file, entity, predicate, and line number information at an
   unification port
``e`` — exception
   prints exception term thrown by the current goal
``E`` — raise exception
   reads and throws an exception term
``=`` — debugging
   prints debugging information
``<`` — write depth
   sets the write term depth (set to ``0`` to reset)
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

.. _debugging_term_write:

Customizing term writing
------------------------

Debugging complex applications often requires customizing term writing.
The available options are limiting the writing depth of large compound
terms and defining the traditional ``portray/1`` to define how a term
should be printed when using the ``p`` command at a leashed port.

.. _debugging_term_write_depth:

Term write depth
~~~~~~~~~~~~~~~~

The terms written by the debugger can be quite large depending on the
application being debugged. As described in the previous section, the
debugger accepts the ``<`` command to set the maximum write term depth
for compound terms. This commmand requires that the used
:term:`backend Prolog compiler` supports the non-standard but common
``max_depth/1`` option for the ``write_term/3`` predicate. When the
compound term being written is deeply nested, the sub-terms are only
written up to the specified depth with the omitted sub-terms replaced
usually by ``...``. For example:

::

   | ?- write_term([0,1,2,3,4,5,6,7,8,9], [max_depth(5)]).
   
   [0,1,2,3,4|...]
   yes

The default maximum depth depends on the backend. To print compound
terms without a depth limit, set it explicitly to zero if necessary.

.. _debugging_custom_term_writing:

Custom term writing
~~~~~~~~~~~~~~~~~~~

The implicit use of the traditional ``print/1`` predicate (using the
``p`` command) and the ``portray/1`` user-defined hook predicate
requires backend Prolog compiler support for these predicates. See
the documentation of the backend you intend to use for details. As
an example, assuming the following ``portray/1`` definition:

::

   portray(e(V1,V2)) :-
       format('~q ---> ~q~n', [V1,V2]).

Calling the ``print/1`` predicate with e.g. a ``e(x1,x7)`` compound term
argument will output:

.. code-block:: text

   | ?- print(e(x1,x7)).

   x1 ---> x7
   yes

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

The ``(<<)/2`` control construct works by switching the execution context
to the object in the first argument and then compiling and executing the
second argument within that context:

.. code-block:: text

   | ?- broken << (self(Self), sender(Sender), this(This)).

   Self = broken
   Sender = broken
   This = broken

   yes

As exemplified above, the ``(<<)/2`` control construct allows you to call
an object local and private predicates. However, it is important to
stress that we are not bypassing or defeating an object predicate scope
directives. The calls take place within the context of the specified
object, not within the context of the object making the ``(<<)/2`` call.
Thus, the ``(<<)/2`` control construct implements a form of
*execution-context switching*.

The availability of the ``(<<)/2`` control construct is controlled by the
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
tokenization for seven *meta-messages* that cover the most common cases:

``@Message``
   By default, the message is printed as passed to the ``write/1``
   predicate followed by a newline.
``Key-Value``
   By default, the message is printed as ``Key: Value`` followed by a
   newline. The key is printed as passed to the ``write/1`` predicate
   while the value is printed as passed to the ``writeq/1`` predicate.
``Format+Arguments``
   By default, the message is printed as passed to the ``format/2``
   predicate.
``List``
   By default, the list items are printed indented one per line. The
   items are preceded by a dash and can be ``@Message``, ``Key-Value``,
   or ``Format+Arguments`` messages. If that is not the case, the item
   is printed as passed to the ``writeq/1`` predicate.
``Title::List``
   By default, the title is printed followed by a newline and the
   indented list items, one per line. The items are printed as in
   the ``List`` meta message.
``[Stream,Prefix]>>Goal``
   By default, call user-defined printing ``Goal`` in the context of
   ``user``. The use of a lambda expression allows passing the message
   stream and prefix. Printing the prefix is delegated to the goal.
``[Stream]>>Goal``
   By default, call user-defined printing ``Goal`` in the context of
   ``user``. The use of a lambda expression allows passing the message
   stream.

Some simple examples of using these meta-messages:

.. code-block:: text

   | ?- logtalk::print_message(debug, core, @'Phase 1 completed').
   yes

   | ?- logtalk::print_message(debug, core, [Stream]>>write(Stream,foo)).
   yes

   | ?- set_logtalk_flag(debug, on).
   yes

   | ?- logtalk::print_message(debug, core, [Stream]>>write(Stream,foo)).
   foo
   yes

   | ?- logtalk::print_message(debug, core, @'Phase 1 completed').
   >>> Phase 1 completed
   yes

   | ?- logtalk::print_message(debug, core, answer-42).
   >>> answer: 42
   yes

   | ?- logtalk::print_message(debug, core, 'Position: <~d,~d>'+[42,23]).
   >>> Position: <42,23>
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

By default, all debug messages are either printed or skipped, depending on the
:ref:`debug <flag_debug>` and :ref:`optimize <flag_optimize>` flags. When the
code is not compiled in optimal mode, the :doc:`../devtools/debug_messages`
tool allows selectively enabling of debug messages per :term:`component` and
per debug group. For example, to enable all ``debug`` and ``debug(Group)``
messages for the ``parser`` component:

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
conditional compilation of debugging goals. For example, the
:doc:`../libraries/hook_objects` library provides a
:ref:`print_goal_hook <apis:print_goal_hook/0>` object that simplifies
printing entity goals before or after calling them by simply prefixing them
with an operator. See the library and hook object documentation for details.
You can also define your own specialized hook objects for custom debugging
tasks.

.. _debugging_ports_profiling:

Ports profiling
---------------

The Logtalk distribution includes a :doc:`../devtools/ports_profiler` tool
based on the same procedure box model described above. This tool is
specially useful for debugging performance issues (e.g. due to lack of
determinism or unexpected backtracking). See the tool documentation for
details. 

.. _debugging_events:

Debug and trace events
----------------------

The debugging API defines two multifile predicates,
:ref:`logtalk::trace_event/2 <apis:logtalk/0::trace_event/2>` and
:ref:`logtalk::debug_handler/3 <apis:logtalk/0::debug_handler/3>` for handiling
trace and debug events. It also provides a
:ref:`logtalk::debug_handler/1 <apis:logtalk/0::debug_handler/1>` multifile
predicate that allows an object (or a category) to declare itself
as a debug handler provider. The Logtalk ``debugger`` and  ``ports_profiler``
tools are regular applications thar are implemented using this API, which
can also be used to implement alternative or new debugging related tools.
See the API documentation for details and the source code of the ``debugger``
and  ``ports_profiler`` tools for usage examples.

To define a new debug handler provider, add (to an object or category) clauses
for the ``debug_handler/1`` and  ``debug_handler/3`` predicates. For example:

::

   % declare my_debug_handler as a debug handler provider
   :- multifile(logtalk::debug_handler/1).
   logtalk::debug_handler(my_debug_handler).
   
   % handle debug events
   :- multifile(logtalk::debug_handler/3).
   logtalk::debug_handler(my_debug_handler, Event, ExCtx) :-
       debug_handler(Event, ExCtx).
   
   debug_handler(fact(Entity,Fact,Clause,File,Line), ExCtx) :-
       ...
   debug_handler(rule(Entity,Head,Clause,File,Line), ExCtx) :-
       ...
   debug_handler(top_goal(Goal, TGoal), ExCtx) :-
       ...
   debug_handler(goal(Goal, TGoal), ExCtx) :-
       ...

Your debug handler provider should also either automatically call the
:ref:`logtalk::activate_debug_handler/1 <apis:logtalk/0::activate_debug_handler/1>`
and :ref:`logtalk::deactivate_debug_handler/0 <apis:logtalk/0::deactivate_debug_handler/0>`
predicate or provide public predicates to simplify calling these predicates.
For example:

::

   :- public(start/0).
   start :-
      logtalk::activate_debug_handler(my_debug_handler).

   :- public(stop/0).
   stop :-
      logtalk::deactivate_debug_handler.

If you only need to define a trace event handler, then simply define clauses
for the :ref:`logtalk::trace_event/2 <apis:logtalk/0::trace_event/2>` multifile
predicate:

::

   :- multifile(logtalk::trace_event/2).
   :- dynamic(logtalk::trace_event/2).
   
   % the Logtalk runtime calls all defined logtalk::trace_event/2 hooks using
   % a failure-driven loop; thus we don't have to worry about handling all
   % events or failing after handling an event to give other hooks a chance
   logtalk::trace_event(fact(Entity, Fact, N, _, _), _) :-
       ...
   logtalk::trace_event(rule(Entity, Head, N, _, _), _) :-
       ...
