
.. index:: conforms_to_protocol/2-3
.. _predicates_conforms_to_protocol_2_3:

conforms_to_protocol/2-3
========================

Description
-----------

::

   conforms_to_protocol(Object, Protocol)
   conforms_to_protocol(Category, Protocol)

   conforms_to_protocol(Object, Protocol, Scope)
   conforms_to_protocol(Category, Protocol, Scope)

Enumerates, by backtracking, all pairs of entities such that an object
or a category conforms to a protocol. The relation scope is represented
by the atoms ``public``, ``protected``, and ``private``. This predicate
implements a transitive closure for the protocol implementation
relation.

Template and modes
------------------

::

   conforms_to_protocol(?object_identifier, ?protocol_identifier)
   conforms_to_protocol(?category_identifier, ?protocol_identifier)

   conforms_to_protocol(?object_identifier, ?protocol_identifier, ?scope)
   conforms_to_protocol(?category_identifier, ?protocol_identifier, ?scope)

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

   | ?- conforms_to_protocol(list, listp).

   | ?- conforms_to_protocol(list, listp, public).

.. seealso::

   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_implements_protocol_2_3`
