
.. index:: findall/4
.. _methods_findall_4:

findall/4
=========

Description
-----------

::

   findall(Template, Goal, List, Tail)

Variant of the ``findall/3`` method that allows passing the tail of the
results list. It succeeds returning the tail argument when the goal have
no solutions.

This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

Template and modes
------------------

::

   findall(?term, +callable, ?list, +list)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``
Goal is a call to a non-existing predicate:
   ``existence_error(procedure, Predicate)``

Examples
--------

To find all solutions in the context of the object or category containing the call:
   ``findall(Template, Goal, List, Tail)``
To find all solutions of sending a message to :term:`self`:
   ``findall(Template, ::Message, List, Tail)``
To find all solutions of sending a message to an explicit object:
   ``findall(Template, Object::Message, List, Tail)``

.. seealso::

   :ref:`methods_bagof_3`,
   :ref:`methods_findall_3`,
   :ref:`methods_forall_2`,
   :ref:`methods_setof_3`
