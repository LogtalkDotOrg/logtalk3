
.. index:: implements_protocol/2-3
.. _predicates_implements_protocol_2_3:

implements_protocol/2-3
=======================

Description
-----------

::

   implements_protocol(Object, Protocol)
   implements_protocol(Category, Protocol)

   implements_protocol(Object, Protocol, Scope)
   implements_protocol(Category, Protocol, Scope)

Enumerates, by backtracking, all pairs of entities such that an object
or a category implements a protocol. The relation scope is represented
by the atoms ``public``, ``protected``, and ``private``. This predicate
only returns direct implementation relations; it does not implement a
transitive closure.

Template and modes
------------------

::

   implements_protocol(?object_identifier, ?protocol_identifier)
   implements_protocol(?category_identifier, ?protocol_identifier)

   implements_protocol(?object_identifier, ?protocol_identifier, ?scope)
   implements_protocol(?category_identifier, ?protocol_identifier, ?scope)

Errors
------

Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Object)``
Category is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Category)``
Protocol is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Protocol)``
Scope is neither a variable nor an atom:
   ``type_error(atom, Scope)``
Scope is an atom but an invalid entity scope:
   ``domain_error(scope, Scope)``

Examples
--------

::

   | ?- implements_protocol(list, listp).

   | ?- implements_protocol(list, listp, public).

See also
--------

:ref:`predicates_current_protocol_1`,
:ref:`predicates_conforms_to_protocol_2_3`
