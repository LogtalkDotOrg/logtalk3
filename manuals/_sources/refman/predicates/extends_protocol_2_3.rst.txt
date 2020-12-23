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


.. index:: pair: extends_protocol/2-3; Built-in predicate
.. _predicates_extends_protocol_2_3:

``extends_protocol/2-3``
========================

Description
-----------

::

   extends_protocol(Protocol, ParentProtocol)
   extends_protocol(Protocol, ParentProtocol, Scope)

Enumerates, by backtracking, all pairs of protocols such that the first
one extends the second. The relation scope is represented by the atoms
``public``, ``protected``, and ``private``.

Modes and number of proofs
--------------------------

::

   extends_protocol(?protocol_identifier, ?protocol_identifier) - zero_or_more
   extends_protocol(?protocol_identifier, ?protocol_identifier, ?scope) - zero_or_more

Errors
------

| ``Protocol`` is neither a variable nor a valid protocol identifier:
|     ``type_error(protocol_identifier, Protocol)``
| ``ParentProtocol`` is neither a variable nor a valid protocol identifier:
|     ``type_error(protocol_identifier, ParentProtocol)``
| ``Scope`` is neither a variable nor an atom:
|     ``type_error(atom, Scope)``
| ``Scope`` is an atom but an invalid entity scope:
|     ``domain_error(scope, Scope)``

Examples
--------

::

   % enumerate the protocols extended by the listp protocol:
   | ?- extends_protocol(listp, Protocol).

   % enumerate protocols that privately extend the termp protocol:
   | ?- extends_protocol(Protocol, termp, private).

.. seealso::

   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_implements_protocol_2_3`,
   :ref:`predicates_conforms_to_protocol_2_3`
