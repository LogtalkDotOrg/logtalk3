..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


Profiling programs
==================

In this example, we will illustrate the use of:

-  events
-  monitors

by defining a simple profiler that prints the starting and ending time
for processing a message sent to an object.

.. _profiling_events:

Messages as events
------------------

In a pure object-oriented system, all computations start by sending
messages to objects. We can thus define an *event* as the sending of a
message to an object. An event can then be specified by the tuple
``(Object, Message, Sender)``. This definition can be refined by
interpreting the sending of a message and the return of the control to
the object that has sent the message as two distinct events. We call
these events respectively ``before`` and ``after``. Therefore, we end up
by representing an event by the tuple
``(Event, Object, Message, Sender)``. For instance, if we send the
message:

.. code-block:: text

   | ?- foo::bar(X).

   X = 1
   yes

the two corresponding events will be:

.. code-block:: text

   (before, foo, bar(X), user)
   (after, foo, bar(1), user)

Note that the second event is only generated if the message succeeds. If
the message as a goal has multiple solutions, then an ``after`` event
will be generated for each solution.

Events are automatically generated by the message-sending mechanisms for
each public message sent using the
:ref:`control_send_to_object_2` operator.

.. _profiling_monitors:

Profilers as monitors
---------------------

A monitor is an object that reacts whenever a spied event occurs. The
monitor actions are defined by two event handlers:
:ref:`methods_before_3` for ``before`` events
and :ref:`methods_after_3` for ``after``
events. These predicates are automatically called by the message-sending
mechanisms when an event registered for the monitor occurs. These event
handlers are declared as public predicates in the ``monitoring``
built-in protocol.

In our example, we need a way to get the current time before and after
we process a message. We will assume that we have a ``time`` object
implementing a ``cpu_time/1`` predicate that returns the current CPU
time for the Prolog session:

::

   :- object(time).

       :- public(cpu_time/1).
       :- mode(cpu_time(-number), one).
       ...

   :- end_object.

Our profiler will be named ``stop_watch``. It must define event handlers
for the ``before`` and ``after`` events that will print the event
description (object, message, and sender) and the current time:

::

   :- object(stop_watch,
       % event handler predicates protocol
       implements(monitoring)).

       :- uses(time, [cpu_time/1]).

       before(Object, Message, Sender) :-
           write(Object), write(' <-- '), writeq(Message),
           write(' from '), write(Sender), nl, write('STARTING at '),
           cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

       after(Object, Message, Sender) :-
           write(Object), write(' <-- '), writeq(Message),
           write(' from '), write(Sender), nl, write('ENDING at '),
           cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

   :- end_object.

After compiling and loading the ``stop_watch`` object (and the objects
that we want to profile), we can use the :ref:`predicates_define_events_5`
built-in predicate to set up our profiler. For example, to profile all
messages that are sent to the object ``foo``, we need to call the goal:

.. code-block:: text

   | ?- define_events(_, foo, _, _, stop_watch).

   yes

This call will register ``stop_watch`` as a monitor to all messages sent
to object ``foo``, for both ``before`` and ``after`` events. Note that
we say "as a monitor", not "the monitor": we can have any number of
monitors over the same events.

From now on, every time we sent a message to ``foo``, the ``stop_watch``
monitor will print the starting and ending times for the message
execution. For instance:

.. code-block:: text

   | ?- foo::bar(X).

   foo <-- bar(X) from user
   STARTING at 12.87415 seconds
   foo <-- bar(1) from user
   ENDING at 12.87419 seconds

   X = 1
   yes

To stop profiling the messages sent to ``foo`` we use the
:ref:`predicates_abolish_events_5` built-in predicate:

.. code-block:: text

   | ?- abolish_events(_, foo, _, _, stop_watch).

   yes

This call will abolish all events defined over the object ``foo``
assigned to the ``stop_watch`` monitor.

Summary
-------

-  An event is defined as the sending of a (public) message to an
   object.

-  There are two kinds of events: ``before`` events, generated before a
   message is processed, and ``after`` events, generated after the
   message processing has completed successfully.

-  Any object can be declared as a monitor to any event. A monitor shall
   reference the ``monitoring`` built-in protocol in the object opening
   directive.

-  A monitor defines event handlers, the predicates
   :ref:`methods_before_3` and :ref:`methods_after_3`, that are
   automatically called by the runtime engine when a spied event occurs.

-  Three built-in predicates, :ref:`predicates_define_events_5`,
   :ref:`predicates_current_event_5`, and
   :ref:`predicates_abolish_events_5`,
   enables us to define, query, and abolish both events and monitors.
