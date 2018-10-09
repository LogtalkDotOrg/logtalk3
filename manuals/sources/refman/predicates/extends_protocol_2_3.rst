
.. index:: extends_protocol/2-3
.. _predicates_extends_protocol_2_3:

extends_protocol/2-3
====================

Description
-----------

::

   extends_protocol(Protocol1, Protocol2)
   extends_protocol(Protocol1, Protocol2, Scope)

Enumerates, by backtracking, all pairs of protocols such that the first
one extends the second. The relation scope is represented by the atoms
``public``, ``protected``, and ``private``.

Template and modes
------------------

::

   extends_protocol(?protocol_identifier, ?protocol_identifier)
   extends_protocol(?protocol_identifier, ?protocol_identifier, ?scope)

Errors
------

Protocol1 is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Protocol1)``
Protocol2 is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Protocol2)``
Scope is neither a variable nor an atom:
   ``type_error(atom, Scope)``
Scope is an atom but an invalid entity scope:
   ``domain_error(scope, Scope)``

Examples
--------

::

   | ?- extends_protocol(listp, Protocol).

   | ?- extends_protocol(Protocol, termp, private).

.. seealso::

   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_implements_protocol_2_3`,
   :ref:`predicates_conforms_to_protocol_2_3`
