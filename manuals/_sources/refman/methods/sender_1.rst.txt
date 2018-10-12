
.. index:: sender/1
.. _methods_sender_1:

sender/1
========

Description
-----------

::

   sender(Sender)

Returns the object that has sent the message under processing. This
private method is translated into a unification between its argument and
the corresponding implicit context argument in the predicate containing
the call. This unification occurs at the clause head when the argument
is not instantiated (the most common case).

Template and modes
------------------

::

   sender(?object_identifier)

Errors
------

(none)

Examples
--------

::

   % after compilation, the write/1 call will be the first goal on the clause body:

   test :-
       sender(Sender),
       write('executing a method to answer a message sent by '),
       writeq(Sender), nl.

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_parameter_2`,
   :ref:`methods_self_1`,
   :ref:`methods_this_1`
