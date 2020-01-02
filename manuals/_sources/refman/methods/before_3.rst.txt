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


.. index:: pair: before/3; Built-in method
.. _methods_before_3:

before/3
========

Description
-----------

::

   before(Object, Message, Sender)

User-defined method for handling *before* events. This method is
declared in the :ref:`monitoring <apis:monitoring/0>` built-in protocol
as a public predicate and automatically called by the runtime for messages
sent using the :ref:`control_send_to_object_2` control construct from within
objects compiled with the :ref:`events <flag_events>` flag set to ``allow``.

Note that you can make this predicate scope protected or private by using,
respectively, :ref:`protected or private implementation <inheritance_scope>`
of the ``monitoring`` protocol.

Modes and number of proofs
--------------------------

::

   before(?object_identifier, ?callable, ?object_identifier) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- object(...,
       implements(monitoring),
       ...).

       % write a log message when a message is sent:
       before(Object, Message, Sender) :-
           writeq(Object), write('::'), writeq(Message),
           write(' from '), writeq(Sender), nl.

.. seealso::

   :ref:`methods_after_3`,
   :ref:`predicates_abolish_events_5`,
   :ref:`predicates_current_event_5`,
   :ref:`predicates_define_events_5`
