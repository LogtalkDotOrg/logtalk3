
.. index:: create_protocol/3
.. _predicates_create_protocol_3:

create_protocol/3
=================

Description
-----------

::

   create_protocol(Identifier, Relations, Directives)

Creates a new, dynamic, protocol. This predicate is often used as a
primitive to implement high-level protocol creation methods.

Note that, when opting for runtime generated protocol identifiers, it's
possible to run out of identifiers when using a back-end Prolog compiler
with bounded integer support. The portable solution, when creating a
large number of dynamic protocols in long-running applications, is to
recycle, whenever possible, the identifiers.

When using Logtalk multi-threading features, predicates calling this
built-in predicate may need to be declared synchronized in order to
avoid race conditions.

Template and modes
------------------

::

   create_protocol(?protocol_identifier, @list(protocol_relation), @list(protocol_directive))

Errors
------

Either Relations or Directives is a variable:
   ``instantiation_error``
Identifier is neither a variable nor a valid protocol identifier:
   ``type_error(protocol_identifier, Identifier)``
Identifier is already in use:
   ``permission_error(modify, category, Identifier)``
   ``permission_error(modify, object, Identifier)``
   ``permission_error(modify, protocol, Identifier)``
Relations is neither a variable nor a proper list:
   ``type_error(list, Relations)``
Repeated entity relation clause:
   ``permission_error(repeat, entity_relation, extends/1)``
Directives is neither a variable nor a proper list:
   ``type_error(list, Directives)``

Examples
--------

::

   | ?- create_protocol(
           logging,
           [extends(monitoring)],
           [public([log_file/1, log_on/0, log_off/0])]
        ).

.. seealso::

   :ref:`predicates_abolish_protocol_1`,
   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_protocol_property_2`,
   :ref:`predicates_conforms_to_protocol_2_3`,
   :ref:`predicates_extends_protocol_2_3`,
   :ref:`predicates_implements_protocol_2_3`
