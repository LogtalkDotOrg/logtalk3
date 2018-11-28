..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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

The Logtalk distribution includes in its ``tools`` directory a
command-line debugger, implemented as a Logtalk application. It can be
loaded by typing:

.. code-block:: text

   | ?- logtalk_load(debugger(loader)).

This tool implements debugging features similar to those found on most
Prolog systems. There are some differences, however, between the usual
implementation of Prolog debuggers and the current implementation of the
Logtalk debugger that you should be aware. First, unlike some Prolog
debuggers, the Logtalk debugger is not built-in but a regular Logtalk
application using documented debugging hook predicates. This translates
to a different, although similar, set of debugging features when compared
with some of the more sophisticated Prolog debuggers. Second, debugging is
only possible for entities compiled in debug mode. When compiling an entity
in debug mode, Logtalk decorates clauses with source information to allow
tracing of the goal execution. Third, implementation of spy points allows
the user to specify the execution context for entering the debugger. This
feature is a consequence of the encapsulation of predicates inside objects.

.. _programming_debug_mode:

Compiling source files and entities in debug mode
-------------------------------------------------

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
recompile all loaded files (that were loaded in normal mode) in debug mode:

.. code-block:: text

   | ?- logtalk_make(debug).

With most backend Prolog compilers, the ``{+d}`` top-level shortcut can also
be used.

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

.. _programming_box_model:

Procedure Box model
-------------------

Logtalk uses a *Procedure Box model* similar to those found on most
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

Line number and predicate spy points are specified using the debuuger
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

The debugger is evoked whenever the execution context is true and when
the spy point goal unifies with the goal currently being executed.
Variable bindings resulting from the unification between the current
goal and the goal argument are discarded. The user may establish any
number of context spy points as necessary. For example, in order to call
the debugger whenever a predicate defined on an object named ``foo`` is
called we may define the following spy point:

.. code-block:: text

   | ?- debugger::spy(_, foo, _, _).

   Spy point set.
   yes

For example, we can spy all calls to a ``foo/2`` predicate by setting
the condition:

.. code-block:: text

   | ?- debugger::spy(_, _, _, foo(_, _)).

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

Note that, when tracing, spy points will be ignored. While tracing, the
debugger will pause for user input at each leashed port, printing an
informative message with the port name and the current goal. Before the
port number, when a spy point is set for the current clause or goal, the
debugger will print a ``#`` character for line number spy points, a
``+`` character for predicate spy points, and a ``*`` character for
context spy points. The debugger also provides determinism information
by prefixing the ``exit`` port with a ``*`` character when a call
succeeds with choice-points pending. After the port name, the debugger
prints the goal invocation number. This invocation number is unique and
can be used to correlate the port trace messages.

To stop tracing and turning off the debugger, write:

.. code-block:: text

   | ?- debugger::notrace.

   yes

.. _programming_debug:

Debugging using spy points
--------------------------

Tracing a program execution may generate large amounts of debugging
data. Debugging using spy points allows the user to concentrate its
attention in specific points of its code. To start a debugging session
using spy points, write:

.. code-block:: text

   | ?- debugger::debug.

   yes

At the beginning of a port description, the debugger will print a ``#``,
``+``, or ``*`` character before the current goal if there is,
respectively, a line number, a predicate, or a context spy point
defined.

To stop the debugger, write:

::

   | ?- debugger::nodebug.

   yes

Note that stopping the debugger does not remove any defined spy points.

.. _programming_commands:

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

.. _programming_context:

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

.. _programming_hooks:

Using compilation hooks and term expansion for debugging
--------------------------------------------------------

It is possible to use compilation hooks and the term expansion mechanism
for conditional compilation of debugging goals. Assume that we chose the
predicate ``debug/1`` to represent debug goals. For example:

::

   member(Head, [Head| _]) :-
       debug((write('Base case: '), writeq(member(Head, [Head| _])))).
   member(Head, [_| Tail]) :-
       debug((write('Recursive case: '), writeq(member(Head, Tail)))),
       member(Head, Tail).

When debugging, we want to call the argument of the predicate
``debug/1``. This can be easily accomplished by defining a hook object
containing the following definition for ``goal_expansion/2``:

::

   goal_expansion(debug(Goal), Goal).

When not debugging, we can use a second hook object to discard the
``debug/1`` calls by defining the predicate ``goal_expansion/2`` as
follows:

::

   goal_expansion(debug(_), true).

The Logtalk compiler automatically removes any redundant calls to the
built-in predicate ``true/0`` when compiling object predicates.

.. _programming_debugging_messages:

Debugging messages
------------------

Calls to the ``logtalk::print_message/3`` predicate where the message
kind is either ``debug`` or ``debug(_)`` are only printed, by default,
when the :ref:`debug <flag_debug>` flag is turned on. Note that using
these messages does not require compiling the code in debug mode, only
turning on the flag. To avoid having to define
:ref:`methods_message_tokens_2` grammar rules for
translating each debug message, Logtalk provides default tokenization
for four *meta-messages* that cover the most common cases:

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
