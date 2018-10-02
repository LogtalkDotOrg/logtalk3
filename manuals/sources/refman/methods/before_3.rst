
.. index:: before/3
.. _methods_before_3:

before/3
========

Description
-----------

::

   before(Object, Message, Sender)

User-defined method for handling ``before`` events. This method is
declared in the ``monitoring`` built-in protocol as a public predicate.
Note that you can make its scope protected or private by using,
respectively, protected or private implementation of the ``monitoring``
protocol.

Template and modes
------------------

::

   before(?object_identifier, ?callable, ?object_identifier)

Errors
------

``(none)``

Examples
--------

::

   :- object(...,
       implements(monitoring),
       ...).

       before(Object, Message, Sender) :-
           writeq(Object), write('::'), writeq(Message),
           write(' from '), writeq(Sender), nl.

See also
--------

:ref:`methods_after_3`,
:ref:`predicates_abolish_events_5`,
:ref:`predicates_current_event_5`,
:ref:`predicates_define_events_5`
