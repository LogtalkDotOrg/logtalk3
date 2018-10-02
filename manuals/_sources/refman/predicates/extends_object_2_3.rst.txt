
.. index:: extends_object/2-3
.. _predicates_extends_object_2_3:

extends_object/2-3
==================

Description
-----------

::

   extends_object(Prototype, Parent)
   extends_object(Prototype, Parent, Scope)

Enumerates, by backtracking, all pairs of objects such that the first
one extends the second. The relation scope is represented by the atoms
``public``, ``protected``, and ``private``.

Template and modes
------------------

::

   extends_object(?object_identifier, ?object_identifier)
   extends_object(?object_identifier, ?object_identifier, ?scope)

Errors
------

Prototype is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Prototype)``
Parent is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Parent)``
Scope is neither a variable nor an atom:
   ``type_error(atom, Scope)``
Scope is an atom but an invalid entity scope:
   ``domain_error(scope, Scope)``

Examples
--------

::

   | ?- extends_object(Object, state_space).

   | ?- extends_object(Object, list, public).

See also
--------

:ref:`predicates_current_object_1`,
:ref:`predicates_instantiates_class_2_3`,
:ref:`predicates_specializes_class_2_3`
