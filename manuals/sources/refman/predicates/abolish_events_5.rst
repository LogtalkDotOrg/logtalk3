..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: abolish_events/5; Built-in predicate
.. _predicates_abolish_events_5:

``abolish_events/5``
====================

Description
-----------

::

   abolish_events(Event, Object, Message, Sender, Monitor)

Abolishes all matching events. The two types of events are represented
by the atoms ``before`` and ``after``. When the predicate is called with
the first argument unbound, both types of events are abolished.

Modes and number of proofs
--------------------------

::

   abolish_events(@term, @term, @term, @term, @term) - one

Errors
------

| ``Event`` is neither a variable nor a valid event identifier:
|     ``type_error(event, Event)``
| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Message`` is neither a variable nor a callable term:
|     ``type_error(callable, Message)``
| ``Sender`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Sender)``
| ``Monitor`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Monitor)``

Examples
--------

::

   % abolish all events for messages sent to the "list"
   % object being monitored by the "debugger" object:
   | ?- abolish_events(_, list, _, _, debugger).

.. seealso::

   :ref:`predicates_current_event_5`,
   :ref:`predicates_define_events_5`,
   :ref:`methods_before_3`,
   :ref:`methods_after_3`
