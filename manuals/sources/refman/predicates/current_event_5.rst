
.. index:: current_event/5
.. _predicates_current_event_5:

current_event/5
===============

Description
-----------

::

   current_event(Event, Object, Message, Sender, Monitor)

Enumerates, by backtracking, all defined events. The two types of events
are represented by the atoms ``before`` and ``after``.

Template and modes
------------------

::

   current_event(?event, ?term, ?term, ?term, ?object_identifier)

Errors
------

Event is neither a variable nor a valid event identifier:
   ``type_error(event, Event)``
Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Object)``
Message is neither a variable nor a callable term:
   ``type_error(callable, Message)``
Sender is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Sender)``
Monitor is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Monitor)``

Examples
--------

::

   | ?- current_event(Event, Object, Message, Sender, debugger).

See also
--------

:ref:`predicates_abolish_events_5`,
:ref:`predicates_define_events_5`,
:ref:`methods_before_3`,
:ref:`methods_after_3`
