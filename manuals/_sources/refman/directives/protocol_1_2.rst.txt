
.. _directives_protocol_1_2:

protocol/1-2
============

Description
-----------

::

   protocol(Protocol)

   protocol(Protocol,
       extends(Protocols))

Starting protocol directive.

Template and modes
------------------

::

   protocol(+protocol_identifier)

   protocol(+protocol_identifier,
       extends(+extended_protocols))

Examples
--------

::

   :- protocol(listp).

   :- protocol(listp,
       extends(compoundp)).

   :- protocol(queuep,
       extends(protected::listp)).

See also
--------

:ref:`directives_end_protocol_0`
