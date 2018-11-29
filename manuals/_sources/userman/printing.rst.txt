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


.. _printing_printing:

Printing messages and asking questions
======================================

Applications, components, and libraries often print all sorts of messages.
These include banners, logging, debugging, and computation results messages
but also, in some cases, user interaction messages. However, the authors of
applications, components, and libraries often cannot anticipate the context
where their software will be used and thus decide which and when messages
should be displayed, suppressed, or diverted. Consider the different components
in a Logtalk application development and deployment. At the bottom level,
you have the Logtalk compiler and runtime. The compiler writes
messages related to e.g. compiling and loading files, compiling
entities, compilation warnings and errors. The runtime may write
banner messages and handles execution errors that may result in printing
human-level messages. The development environment can be console-based
or you may be using a GUI tool such as PDT. In the latter case, PDT
needs to intercept the Logtalk compiler and runtime messages to present
the relevant information using its GUI. Then you have all the other
components in a typical application. For example, your own libraries and
third-party libraries. The libraries may want to print messages on its
own, e.g. banners, debugging information, or logging information. As you
assemble all your application components, you want to have the final
word on which messages are printed, where, an in what conditions.
Uncontrolled message printing by libraries could potentially
disturb application flow, expose implementation details, spam the user
with irrelevant details, or break user interfaces.

The solution is to decouple the calls to print a message from the actual
printing of the output text. The same is true for calls to read user input.
By decoupling the call to input some data from the actual read of the data,
we can easily switched e.g. from a command-line interface to a GUI input
dialog or even automate providing the data (e.g. when automating testing
of user interaction).

Logtalk provides a solution based on the *structured message printing
mechanism* that was introduced by Quintus Prolog, where it was apparently
implemented by Dave Bowen (thanks to Richard O'Keefe for the historical bits).
This mechanism gives the programmer full control of message printing,
allowing it to filter, rewrite, or redirect any message.
Variations of this mechanism can also be found in some Prolog systems
including SICStus Prolog, SWI-Prolog, and YAP. Based on this mechanism,
Logtalk introduces an extension that also allows abstracting asking a user
for input. Both mechanisms are implemented by the :ref:`logtalk <apis:logtalk/0>`
built-in object and described in this section. The message printing mechanism is
widely used by the Logtalk compiler itself and by the developer tools.

.. _printing_messages:

Printing messages
-----------------

The main predicate for printing a message is
:ref:`logtalk::print_message/3 <apis:logtalk/0::print_message/3>`.
A simple example, using the Logtalk runtime is:

.. code-block:: text

   | ?- logtalk::print_message(banner, core, banner).

   Logtalk 3.22.0
   Copyright (c) 1998-2018 Paulo Moura
   yes

The first argument of the predicate is the kind of message that we
want to print. In this case, we use ``banner`` to indicate that
we are printing a product name and copyright banner. An extensive
list of message kinds is supported by default:

``banner``
   banner messages (used e.g. when loading tools or main application
   components; can be suppressed by setting the :ref:`report <flag_report>`
   flag to ``warnings`` or ``off``)
``help``
   messages printed in reply for the user asking for help (mostly for
   helping port existing Prolog code)
``information`` and ``information(Group)``
   messages usually printed in reply to a user request for information
``silent`` and ``silent(Group)``
   not printed by default (but can be intercepted using the
   ``message_hook/4`` predicate)
``comment`` and ``comment(Group)``
   useful but usually not essential messages (can be suppressed by
   setting the :ref:`report <flag_report>` flag to ``warnings`` or ``off``)
``warning`` and ``warning(Group)``
   warning messages (generated e.g. by the compiler; can be suppressed
   by turning off the :ref:`report <flag_report>` flag)
``error`` and ``error(Group)``
   error messages (generated e.g. by the compiler)
``debug, debug(Group)``
   debugging messages
``question, question(Group)``
   questions to a user

Using a compound term allows easy partitioning of messages of the same kind
in different groups. Note that you can define your own alternative message
kind identifiers, for your own components, together with suitable definitions
for their associated prefixes and output streams.

The second argument of ``print_message/3`` is new to Logtalk and represents
the *component* defining the message being printed. Here *component* is a
generic term that can designate e.g a tool, a library, or some sub-system
in a large application. In our example, the component name is ``core``,
identifying the Logtalk compiler/runtime. This argument was introduced to
simplify programming-in-the-large by allowing easy filtering of all messages
from a specific component or library and also avoiding conflicts when two
components happen to define the same message term (e.g. ``banner``). Users
should choose and use a unique name for their components, which usually is
the name of the component itself. For example, all messages from the
``lgtunit`` tool use ``lgtunit`` for the component argument.

The third argument of ``print_message/3`` is the message itself represented
by a term. In this case, the message term is ``banner``. Using a
term to represent a message instead of a string with the message text itself
have significant advantages. Notably, it simplifies machine-processing and
allows using a compound term for easy parameterization of the message text.
For example:

.. code-block:: text

   | ?- logtalk::print_message(comment, core, redefining_entity(object, foo)).

   % Redefining object foo
   yes

Localization of messages also become possible without changing the
``print_message/3`` predicate calls.

Message tokenization
--------------------

The advantages of using message terms require a solution for generating
the actual messages text. This is supported by defining grammar rules for
the :ref:`logtalk::message_tokens//2 <apis:logtalk/0::message_tokens//2>`
multifile non-terminal, which translates a message term, for a given
component, to a list of tokens. For example:

::

   :- multifile(logtalk::message_tokens//2).
   :- dynamic(logtalk::message_tokens//2).

   logtalk::message_tokens(redefining_entity(Type, Entity), core) -->
       ['Redefining ~w ~q'-[Type, Entity], nl].

The following tokens can be used when translating a message:

``at_same_line``
   Signals a following part to a multi-part message with no line break
   in between; this token is ignored when it's not the first in the list
   of tokens
``flush``
   Flush the output stream (by calling the ``flush_output/1`` standard
   predicate)
``nl``
   Change line in the output stream
``Format-Arguments``
   ``Format`` must be an atom and ``Arguments`` must be a list of format
   arguments (the token arguments are passed to a call to the
   ``format/3`` de facto standard predicate)
``term(Term, Options)``
   ``Term`` can be any term and ``Options`` must be a list of valid
   ``write_term/3`` output options (the token arguments are passed to a
   call to the ``write_term/3`` standard predicate)
``ansi(Attributes, Format, Arguments)``
   Taken from SWI-Prolog; by default, do nothing; can be used for styled
   output
``begin(Kind, Var)``
   Taken from SWI-Prolog; by default, do nothing; can be used together
   with ``end(Var)`` to wrap a sequence of message tokens
``end(Var)``
   Taken from SWI-Prolog; by default, do nothing

The ``logtalk`` object also defines public predicates for printing a list
of tokens, for hooking into printing an individual token, and for setting
default output stream and message prefixes. For example, the SWI-Prolog
adapter file uses the print message token hook predicate to enable coloring
of messages printed on a console.

Define tokenization rules for every message is not always necessary, however.
Logtalk defines several *meta-messages* that are handy for simple cases and
temporary messages only used to help developing, notably debugging messages.
See the :ref:`programming_debugging_messages` section and the
:ref:`logtalk built-in object <apis:logtalk/0>` remarks section for details.

Intercepting messages
---------------------

Calls to the :ref:`logtalk::print_message/3 <apis:logtalk/0::print_message/3>`
predicate can be intercepted by defining clauses for the
:ref:`logtalk::message_hook/4 <apis:logtalk/0::message_hook/4>` multifile
hook predicate. This predicate can suppress, rewrite, and divert messages.

As a first example, assume that you want to make Logtalk startup less verbose
by suppressing printing of the default compiler flag values. This can be
easily accomplished by defining the following category in a settings file:

::

   :- category(my_terse_logtalk_startup_settings).
   
       :- multifile(logtalk::message_hook/4).
       :- dynamic(logtalk::message_hook/4).
   
       logtalk::message_hook(default_flags, comment(settings), core, _).
   
   :- end_category.

The printing message mechanism automatically calls the ``message_hook/4``
hook predicate. When this call succeeds, the mechanism assumes that the
message have been successfully handled.

As another example, assume that you want to print all otherwise silent
compiler messages:

::

   :- category(my_verbose_logtalk_message_settings).
   
       :- multifile(logtalk::message_hook/4).
       :- dynamic(logtalk::message_hook/4).
   
       logtalk::message_hook(_Message, silent, core, Tokens) :-
           logtalk::message_prefix_stream(comment, core, Prefix, Stream),
           logtalk::print_message_tokens(Stream, Prefix, Tokens).
   
       logtalk::message_hook(_Message, silent(Key), core, Tokens) :-
           logtalk::message_prefix_stream(comment(Key), core, Prefix, Stream),
           logtalk::print_message_tokens(Stream, Prefix, Tokens).
   
   :- end_category.

.. _printing_questions:

Asking questions
----------------

Logtalk features a *structured question asking* mechanism that
complements the message printing mechanism. This feature provides an
abstraction for the common task of asking a user a question and reading
back its reply. By default, this mechanism writes the question, writes a
prompt, and reads the answer from the current user input and output
streams but allows both steps to be intercepted, filtered, rewritten,
and redirected. Two typical examples are using a GUI dialog for asking
questions and automatically providing answers to specific questions.

The question asking mechanism works in tandem with the message printing
mechanism, using it to print the question text and a prompt. It provides
an asking predicate and a hook predicate, both declared and defined in
the ``logtalk`` built-in object. The asking predicate,
:ref:`ask_question(Kind, Component, Question, Check, Answer) <methods_ask_question_5>`,
is used for ask a question and read the answer. The hook predicate,
:ref:`question_hook(Question, Kind, Component, Tokens, Check, Answer) <methods_question_hook_6>`,
is used for intercepting questions. The ``Kind`` argument is used to
represent the nature of the question being asked. Its default value is
``question`` but it can be any atom or compound term, e.g.
``question(parameters)``. Using a compound term allows easy partitioning
of messages of the same kind in different groups. The ``Check`` argument
is a closure that is converted into a checking goal taking as argument
the user answer. The ``ask_question/5`` implements a read loop that
terminates when this checking predicate is true. The question itself is
a term that is translated into printing tokens using the
``message_tokens/2`` multifile predicate described above.

There is also a user-defined multifile predicate for setting default
prompt and input streams,
``question_prompt_stream(Kind, Component, Prompt, Stream)`` :ref:`methods_question_prompt_stream_4`.

A usage example of this mechanism can be found in the ``debugger`` tool
where it's used to abstract the user interaction when tracing a goal
execution in debug mode.
