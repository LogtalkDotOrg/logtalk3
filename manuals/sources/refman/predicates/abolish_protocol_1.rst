
.. index:: type_error/2
.. _predicates_abolish_protocol_1:

abolish_protocol/1
==================

Description
-----------

::

   abolish_protocol(Protocol)

Abolishes a dynamic protocol.

Template and modes
------------------

::

   abolish_protocol(@protocol_identifier)

Errors
------

Protocol is a variable:
   ``instantiation_error``
Protocol is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Protocol)``
Protocol is an identifier of a static protocol:
   ``permission_error(modify, static_protocol, Protocol)``
Protocol does not exist:
   ``existence_error(protocol, Protocol)``

Examples
--------

::

   | ?- abolish_protocol(listp).

See also
--------

:ref:`predicates_create_protocol_3`,
:ref:`predicates_current_protocol_1`,
:ref:`predicates_protocol_property_2`,
:ref:`predicates_conforms_to_protocol_2_3`,
:ref:`predicates_extends_protocol_2_3`,
:ref:`predicates_implements_protocol_2_3`
