..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: abolish_protocol/1; Built-in predicate
.. _predicates_abolish_protocol_1:

``abolish_protocol/1``
======================

Description
-----------

::

   abolish_protocol(Protocol)

Abolishes a dynamic protocol. The protocol identifier can then be reused when creating a new protocol.

Modes and number of proofs
--------------------------

::

   abolish_protocol(@protocol_identifier) - one

Errors
------

| ``Protocol`` is a variable:
|     ``instantiation_error``
| ``Protocol`` is neither a variable nor a valid protocol identifier:
|     ``type_error(protocol_identifier, Protocol)``
| ``Protocol`` is an identifier of a static protocol:
|     ``permission_error(modify, static_protocol, Protocol)``
| ``Protocol`` does not exist:
|     ``existence_error(protocol, Protocol)``

Examples
--------

::

   | ?- abolish_protocol(listp).

.. seealso::

   :ref:`predicates_create_protocol_3`,
   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_protocol_property_2`,
   :ref:`predicates_conforms_to_protocol_2_3`,
   :ref:`predicates_extends_protocol_2_3`,
   :ref:`predicates_implements_protocol_2_3`
