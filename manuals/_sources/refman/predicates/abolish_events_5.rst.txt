
.. index:: abolish_events/5
.. _predicates_abolish_events_5:

abolish_events/5
================

Description
-----------

::

   abolish_events(Event, Object, Message, Sender, Monitor)

Abolishes all matching events. The two types of events are represented
by the atoms ``before`` and ``after``. When the predicate is called with
the first argument unbound, both types of events are abolished.

Template and modes
------------------

::

   abolish_events(@term, @term, @term, @term, @term)

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

   | ?- abolish_events(_, list, _, _, debugger).

.. seealso::

   :ref:`predicates_current_event_5`,
   :ref:`predicates_define_events_5`,
   :ref:`methods_before_3`,
   :ref:`methods_after_3`
