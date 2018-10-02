
.. index:: after/3
.. _methods_after_3:

after/3
=======

Description
-----------

::

   after(Object, Message, Sender)

User-defined method for handling ``after`` events. This method is
declared in the ``monitoring`` built-in protocol as a public predicate.
Note that you can make its scope protected or private by using,
respectively, protected or private implementation of the ``monitoring``
protocol.

Template and modes
------------------

::

   after(?object_identifier, ?callable, ?object_identifier)

Errors
------

``(none)``

Examples
--------

::

   :- object(...,
       implements(monitoring),
       ...).

       after(Object, Message, Sender) :-
           writeq(Object), write('::'), writeq(Message),
           write(' from '), writeq(Sender), nl.

See also
--------

:ref:`methods_before_3`,
:ref:`predicates_abolish_events_5`,
:ref:`predicates_current_event_5`,
:ref:`predicates_define_events_5`
