
.. index:: setof/3
.. _methods_setof_3:

setof/3
=======

Description
-----------

::

   setof(Template, Goal, List)

Collects a set of solutions for the goal for each set of instantiations
of the free variables in the goal. The solutions are sorted using
standard term order. The free variables in the goal are the variables
that occur in the goal but not in the template. Free variables can be
ignored, however, by using the ``^/2`` existential qualifier. For
example, if ``T`` is term containing all the free variables that we want
to ignore, we can write ``T^Goal``. Note that the term ``T`` can be
written as ``V1^V2^...``.

When there are free variables, this method is re-executable on
backtracking. This method fails when there are no solutions, never
returning an empty list.

This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

Template and modes
------------------

::

   setof(@term, +callable, -list)

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

To find a set of solutions in the context of the object or category containing the call:
   ``setof(Template, Goal, List)``
To find a set of solutions of sending a message to :term:`self`:
   ``setof(Template, ::Message, List)``
To find a set of solutions of sending a message to an explicit object:
   ``setof(Template, Object::Message, List)``

.. seealso::

   :ref:`methods_bagof_3`,
   :ref:`methods_findall_3`,
   :ref:`methods_findall_4`,
   :ref:`methods_forall_2`
