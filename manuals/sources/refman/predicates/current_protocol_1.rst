
.. index:: current_protocol/1
.. _predicates_current_protocol_1:

current_protocol/1
==================

Description
-----------

::

   current_protocol(Protocol)

Enumerates, by backtracking, all currently defined protocols. All
protocols are found, either static, dynamic, or built-in.

Template and modes
------------------

::

   current_protocol(?protocol_identifier)

Errors
------

Protocol is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Protocol)``

Examples
--------

::

   | ?- current_protocol(listp).

.. seealso::

   :ref:`predicates_abolish_protocol_1`,
   :ref:`predicates_create_protocol_3`,
   :ref:`predicates_protocol_property_2`,
   :ref:`predicates_conforms_to_protocol_2_3`,
   :ref:`predicates_extends_protocol_2_3`,
   :ref:`predicates_implements_protocol_2_3`
