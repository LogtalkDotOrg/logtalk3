..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _messages_messages:

Messages
========

Messages allows us to ask an object to prove a goal and must always match a
declared predicate within the scope of the *sender* object. Note that sending
a message is fundamentally different from calling a predicate. When calling a
predicate, the caller decides implicitly which predicate definition will be
executed. When sending a message, it is the receiving object, not the sender,
that decides which predicate definition (if any) will be called to answer the
message. The predicate definition that is actually used to answer a message
depends on the relations between the object and its imported categories and
ancestor objects (if any). See the :ref:`inheritance_inheritance` section
for details on the predicate declaration and predicate definition lookup
procedures.

When a message corresponds to a :term:`meta-predicate`, the meta-arguments
are always called in the context of the object (or category) sending the
message.

Logtalk uses nomenclature similar to in other object-oriented programming
languages such as Smalltalk. Therefore, the terms *query* and *message* are
used interchangeably when referring to a declared predicate that is part of
an object interface. Likewise, the terms *predicate* and *method* are used
interchangeably when referring to the predicate definition (inside an object
or category) that is called to answer a message.

.. _messages_operators:

Operators used in message sending
---------------------------------

Logtalk declares the following operators for the message sending control
constructs:

::

   :- op(600, xfy, ::).
   :- op(600,  fy, ::).
   :- op(600,  fy, ^^).

It is assumed that these operators remain active (once the Logtalk
compiler and runtime files are loaded) until the end of the Prolog
session (this is the usual behavior of most Prolog compilers). Note that
these operator definitions are compatible with the predefined operators
in the Prolog ISO standard.

.. _messages_sending:

Sending a message to an object
------------------------------

Sending a message to an object is accomplished by using the
:ref:`control_send_to_object_2` control construct:

::

   ..., Object::Message, ...

The message must match a public predicate declared for the receiving
object. The message may also correspond to a protected or private
predicate if the *sender* matches the predicate scope container. If the
predicate is declared but not defined, the message simply fails (as per
the :term:`closed-world assumption`).

.. _messages_delegating:

Delegating a message to an object
---------------------------------

It is also possible to send a message to an object while preserving the
original *sender* by using the :ref:`control_delegate_message_1` delegation
control construct:

::

   ..., [Object::Message], ....

This control construct can only be used within objects and categories
(in the top-level interpreter, the *sender* is always the pseudo-object
``user`` so using this control construct would be equivalent to use the
``::/2`` message sending control construct).

Sending a message to *self*
---------------------------

While defining a predicate, we sometimes need to send a message to
*self*, i.e., to the same object that has received the original message.
This is done in Logtalk through the
:ref:`control_send_to_self_1` control construct:

::

   ..., ::Message, ....

The message must match either a public or protected predicate declared for
the receiving object or a private predicate within the scope of the *sender*
otherwise an error will be thrown. If the message is sent from inside a
category or if we are using private inheritance, then the message may also
match a private predicate. Again, if the predicate is declared but not
defined, the message simply fails (as per the :term:`closed-world assumption`).

.. _messages_broadcasting:

Broadcasting
------------

In the Logtalk context, *broadcasting* is interpreted as the sending of
several messages to the same object. This can be achieved by using the
message sending method described above. However, for convenience,
Logtalk implements an extended syntax for message sending that may
improve program readability in some cases. This extended syntax uses the
``(,)/2``, ``(;)/2``, and ``(->)/2`` control constructs. For example, if
we wish to send several messages to the same object, we can write:

.. code-block:: text

   | ?- Object::(Message1, Message2, ...).

This is semantically equivalent to:

.. code-block:: text

   | ?- Object::Message1, Object::Message2, ... .

This extended syntax may also be used with the ``::/1`` message sending
control construct.

.. _messages_super:

Calling imported and inherited predicates
-----------------------------------------

When redefining a predicate, sometimes we need to call the inherited
definition in the new code. This functionality, introduced by the
Smalltalk language through the ``super`` primitive, is available in
Logtalk using the :ref:`control_call_super_1` control construct:

::

   ..., ^^Predicate, ....

Most of the time we will use this control construct by instantiating the
pattern:

::

   Predicate :-
       ...,            % do something
       ^^Predicate,    % call inherited definition
       ... .           % do something more

This control construct is generalized in Logtalk where it may be used to
call any imported or inherited predicate definition. This control
construct may be used within objects and categories. When combined with
:term:`static binding`, this control construct allows imported and inherited
predicates to be called with the same performance of local predicates.
As with the message sending control constructs, the ``^^/1`` call simply
fails when the predicate is declared but not defined (as per the
:term:`closed-world assumption`).

.. _messages_events:

Message sending and event generation
------------------------------------

Assuming the :ref:`events <flag_events>` flag is set to ``allow`` for the
object (or category) sending a message using the 
:ref:`control_send_to_object_2` control construct, two events are generated,
one before and one after the message execution.
Messages that are sent using the
:ref:`control_send_to_self_1` (message to *self*)
control construct or the
:ref:`control_call_super_1` super mechanism
described above do not generate any events. The rationale behind this
distinction is that messages to *self* and *super* calls are only used
internally in the definition of methods or to execute additional
messages with the same target object (represented by *self*). In other
words, events are only generated when using an object's public
interface; they cannot be used to break object encapsulation.

If we need to generate events for a public message sent to *self*, then
we just need to write something like:

::

   Predicate :-
       ...,
       % get self reference
       self(Self),
       % send a message to self using ::/2
       Self::Message,
       ... .

If we also need the sender of the message to be other than the object
containing the predicate definition, we can write:

::

   Predicate :-
       ...,
       % send a message to self using ::/2
       % sender will be the pseudo-object user
       self(Self),
       {Self::Message},
       ... .

When events are not used, is possible to turn off event generation globally
or on a per entity basis by using the ``events`` compiler flag to optimize
message sending performance (see the :ref:`events_events` section for more
details).

.. _messages_from_module:

Sending a message from a module
-------------------------------

Messages can be sent to object from within a Prolog module. Depending on
the backend Prolog system and on the :ref:`optimize <flag_optimize>` flag
being turned on, the messages will use static binding when possible. This
optimization requires the object to be compiled and loaded before the module.
Note that the module can be ``user``. This is usually the case when sending
the message from the top-level interpreter. Thus, the same conditions apply
in this case.

.. warning::

   If you want to benchmark the performance of a message sending goal
   at the top-level interpreter, be careful to check first if the goal
   is pre-compiled to use static binding, otherwise you will also be
   benchmarking the Logtalk compiler itself.

.. _messages_performance:

Message sending performance
---------------------------

For a detailed discussion on message sending performance, see the
:ref:`performance_performance` section.


..
   .. _messages_performance:
   
   Message sending performance
   ---------------------------
   
   Logtalk supports both :term:`static binding` and :term:`dynamic binding`.
   Static binding is used whenever messages are sent (using the ``::/2`` control
   construct) to static objects already loaded and with the
   :ref:`optimize <flag_optimize>` compiler flag turned on. When that is not
   the case (or when using the ``::/1`` control construct), Logtalk uses dynamic
   binding coupled with a caching mechanism that avoids repeated lookups of
   predicate declarations and predicate definitions. This is a solution common
   to other programming languages supporting dynamic binding. :term:`Message
   lookups <message lookup>` are automatically cached the first time a message
   is sent. Cache entries are automatically removed when loading entities or
   using Logtalk dynamic features that invalidate the cached lookups.

   Whenever static binding is used, message sending performance is roughly
   the same as a predicate call in plain Prolog. When discussing Logtalk
   dynamic binding performance, two distinct cases should be considered:
   messages sent by the user from the top-level interpreter and messages
   sent from compiled objects. In addition, the message declaration and
   definition lookups may, or may not be already cached by the runtime
   engine. In what follows, we will assume that the message lookups are
   already cached.
   
   .. _messages_inferences:
   
   Translating message processing to predicate calls
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   In order to better understand the performance trade-offs of using Logtalk
   dynamic binding when compared to plain Prolog or to Prolog module
   systems, is useful to translate message processing in terms of predicate
   calls. However, in doing this, we should keep in mind that the number of
   predicate calls is not necessarily proportional to the time taken to
   execute them.
   
   With event-support turned on, a message sent from a compiled object (or
   category) to another object translates to a minimum of five predicate
   calls:
   
   checking for *before* events
      one call to the built-in predicate ``\+/1`` and a call to its
      argument, assuming that no events are defined
   method call using the cached lookup
      one call to a dynamic predicate (the cache entry)
   checking for *after* events
      one call to the built-in predicate ``\+/1`` and a call to its
      argument, assuming that no events are defined
   
   Given that events can be dynamically defined at runtime, there is no
   room for reducing the number of predicate calls without turning off
   support for event-driven programming. When events are defined, the
   number of predicate calls grows proportional to the number of events and
   event handlers (monitors). Event-driven programming support can be
   switched off for specific object using the
   :ref:`events <flag_events>` compiler flag. Doing so, reduces
   the number of predicate calls from three to just one.
   
   Messages to *self* are transparent regarding events and, as such, imply
   only one predicate call (to the cache entry, a dynamic predicate).
   
   When a message is sent by the user from the top-level interpreter,
   Logtalk needs to perform a runtime translation of the message term in
   order to prove the corresponding goal. Thus, while sending a message
   from a compiled object corresponds to either three predicate calls
   (event-support on) or one predicate call (event-support off), the same
   message sent by the user from the top-level interpreter necessarily
   implies an overhead. Considering the time taken for the user to type the
   goal and read the reply, this overhead is of no practical consequence.
   
   When a message is not cached, the number of predicate calls depends on
   the number of steps needed for the Logtalk runtime engine to lookup the
   corresponding predicate scope declaration (to check if the message is
   valid) and then to lookup a predicate definition for answering the
   message.
   
   .. _messages_cputime:
   
   Processing time
   ~~~~~~~~~~~~~~~
   
   Not all predicate calls take the same time. Moreover, the time taken to
   process a specific predicate call depends on the Prolog compiler
   implementation details. As such, the only valid performance measure is
   the time taken for processing a message.
   
   The usual way of measuring the time taken by a predicate call is to
   repeat the call a number of times and than to calculate the average
   time. A sufficient large number of repetitions would hopefully lead to
   an accurate measure. Care should be taken to subtract the time taken by
   the repetition code itself. In addition, we should be aware of any
   limitations of the predicates used to measure execution times. One way
   to make sense of numbers we get is to repeat the test with the same
   predicate using plain Prolog and with the predicate encapsulated in a
   module.
   
   A simple predicate for helping benchmarking predicate calls could be:
   
   ::
   
      benchmark(N, Goal) :-
          repeat(N),
              call(Goal),
          fail.
   
      benchmark(_, _).
   
   The rational of using a failure-driven loop is to try to avoid any
   interference on our timing measurements from garbage-collection or
   memory expansion mechanisms. Based on the predicate ``benchmark/2``, we
   may define a more convenient predicate for performing our benchmarks.
   For example:
   
   ::
   
      benchmark(Goal) :-
          % some sufficiently large number of repetitions
          N = 10000000,
          write('Number of repetitions: '), write(N), nl,
          % replace by your Prolog-specific predicate
          get_cpu_time(Seconds1),
          benchmark(N, Goal),
          get_cpu_time(Seconds2),
          Average is (Seconds2 - Seconds1)/N,
          write('Average time per call: '), write(Average), write(' seconds'), nl,
          Speed is 1.0/Average,
          write('Number of calls per second: '), write(Speed), nl.
   
   We can get a baseline for our timings by doing:
   
   .. code-block:: text
   
      | ?- benchmark(true).
   
   For comparing message sending performance across several Prolog
   compilers, we would call the ``benchmark/1`` predicate with a suitable
   argument. For example:
   
   .. code-block:: text
   
      | ?- benchmark(list::length([1,2,3,4,5,6,7,8,9,0], _)).
   
   For comparing message sending performance with predicate calls in plain
   Prolog and with calls to predicates encapsulated in modules, we should
   use exactly the same predicate definition in the three cases.
   
   It should be stressed that message sending is only one of the factors
   affecting the performance of a Logtalk application (and often not the
   most important one). The strengths and limitations of the chosen Prolog
   compiler play a crucial role on all aspects of the development,
   reliability, usability, and performance of a Logtalk application. It is
   advisable to take advantage of the Logtalk wide compatibility with most
   Prolog compilers to test for the best match for developing your Logtalk
   applications.
