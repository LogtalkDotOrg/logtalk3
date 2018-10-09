
.. index:: specializes_class/2-3
.. _predicates_specializes_class_2_3:

specializes_class/2-3
=====================

Description
-----------

::

   specializes_class(Class, Superclass)
   specializes_class(Class, Superclass, Scope)

Enumerates, by backtracking, all pairs of objects such that the first
one specializes the second. The relation scope is represented by the
atoms ``public``, ``protected``, and ``private``.

Template and modes
------------------

::

   specializes_class(?object_identifier, ?object_identifier)
   specializes_class(?object_identifier, ?object_identifier, ?scope)

Errors
------

Class is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Class)``
Superclass is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Superclass)``
Scope is neither a variable nor an atom:
   ``type_error(atom, Scope)``
Scope is an atom but an invalid entity scope:
   ``domain_error(scope, Scope)``

Examples
--------

::

   | ?- specializes_class(Subclass, state_space).

   | ?- specializes_class(Subclass, state_space, public).

.. seealso::

   :ref:`predicates_current_object_1`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`
