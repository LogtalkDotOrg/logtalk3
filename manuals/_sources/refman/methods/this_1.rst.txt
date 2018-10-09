
.. index:: this/1
.. _methods_this_1:

this/1
======

Description
-----------

::

   this(This)

Unifies its argument with the identifier of the object for which the
predicate clause whose body is being executed is defined (or the object
importing the category that contains the predicate clause). This private
method is implemented as a unification between its argument and the
corresponding implicit execution-context argument in the predicate
containing the call. This unification occurs at the clause head when the
argument is not instantiated (the most common case). This method is
useful for avoiding hard-coding references to an object identifier or
for retrieving all object parameters with a single call when using
parametric objects.

Template and modes
------------------

::

   this(?object_identifier)

Errors
------

``(none)``

Examples
--------

::

   % after compilation, the write/1 call will be the first goal on the clause body:

   test :-
       this(This),
       write('Using a predicate clause contained in '),
       writeq(This), nl.

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_parameter_2`,
   :ref:`methods_self_1`,
   :ref:`methods_sender_1`
