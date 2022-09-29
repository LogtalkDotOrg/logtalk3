..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


.. rst-class:: align-right

**built-in predicate**

.. index:: pair: define_events/5; Built-in predicate
.. _predicates_define_events_5:

``define_events/5``
===================

Description
-----------

::

   define_events(Event, Object, Message, Sender, Monitor)

Defines a new set of events. The two types of events are represented by
the atoms ``before`` and ``after``. When the predicate is called with
the first argument unbound, both types of events are defined. The object
``Monitor`` must define the event handler methods required by the
``Event`` argument.

Modes and number of proofs
--------------------------

::

   define_events(@term, @term, @term, @term, +object_identifier) - one

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
| ``Monitor`` is a variable:
|     ``instantiation_error``
| ``Monitor`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Monitor)``
| ``Monitor`` does not define the required ``before/3`` method:
|     ``existence_error(procedure, before/3)``
| ``Monitor`` does not define the required ``after/3`` method:
|     ``existence_error(procedure, after/3)``

Examples
--------

::

   % define "debugger" as a monitor for member/2 messages
   % sent to the "list" object:
   | ?- define_events(_, list, member(_, _), _ , debugger).

.. seealso::

   :ref:`predicates_abolish_events_5`,
   :ref:`predicates_current_event_5`,
   :ref:`methods_before_3`,
   :ref:`methods_after_3`
