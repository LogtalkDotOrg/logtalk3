
.. index:: define_events/5
.. _predicates_define_events_5:

define_events/5
===============

Description
-----------

::

   define_events(Event, Object, Message, Sender, Monitor)

Defines a new set of events. The two types of events are represented by
the atoms ``before`` and ``after``. When the predicate is called with
the first argument unbound, both types of events are defined. The object
``Monitor`` must define the event handler methods required by the
``Event`` argument.

Template and modes
------------------

::

   define_events(@term, @term, @term, @term, +object_identifier)

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
Monitor is a variable:
   ``instantiation_error``
Monitor is neither a variable nor a valid object identifier:
   ``existence_error(object_identifier, Monitor)``
Monitor does not define the required ``before/3`` method:
   ``existence_error(procedure, before/3)``
Monitor does not define the required ``after/3`` method:
   ``existence_error(procedure, after/3)``

Examples
--------

::

   | ?- define_events(_, list, member(_, _), _ , debugger).

See also
--------

:ref:`predicates_abolish_events_5`,
:ref:`predicates_current_event_5`,
:ref:`methods_before_3`,
:ref:`methods_after_3`
