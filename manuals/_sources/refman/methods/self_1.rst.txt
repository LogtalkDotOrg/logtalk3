
.. index:: self/1
.. _methods_self_1:

self/1
======

Description
-----------

::

   self(Self)

Returns the object that has received the message under processing. This
private method is translated to a unification between its argument and
the corresponding implicit context argument in the predicate containing
the call. This unification occurs at the clause head when the argument
is not instantiated (the most common case).

Template and modes
------------------

::

   self(?object_identifier)

Errors
------

``(none)``

Examples
--------

::

   test :-
       self(Self),                                   % after compilation, the write/1
       write('executing a method in behalf of '),    % call will be the first goal on
       writeq(Self), nl.                             % the clause body

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_parameter_2`,
   :ref:`methods_sender_1`,
   :ref:`methods_this_1`
