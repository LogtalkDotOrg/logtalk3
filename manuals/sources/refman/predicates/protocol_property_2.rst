
.. index:: protocol_property/2
.. _predicates_protocol_property_2:

protocol_property/2
===================

Description
-----------

::

   protocol_property(Protocol, Property)

Enumerates, by backtracking, the properties associated with the
currently defined protocols. The valid protocol properties are listed in
the language :ref:`grammar_entity_properties`.

Template and modes
------------------

::

   protocol_property(?protocol_identifier, ?protocol_property)

Errors
------

Protocol is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Protocol)``
Property is neither a variable nor a callable term:
   ``type_error(callable, Property)``
Property is a callable term but not a valid protocol property:
   ``domain_error(protocol_property, Property)``

Examples
--------

::

   | ?- protocol_property(listp, Property).

See also
--------

:ref:`predicates_abolish_protocol_1`,
:ref:`predicates_create_protocol_3`,
:ref:`predicates_current_protocol_1`,
:ref:`predicates_conforms_to_protocol_2_3`,
:ref:`predicates_extends_protocol_2_3`,
:ref:`predicates_implements_protocol_2_3`
