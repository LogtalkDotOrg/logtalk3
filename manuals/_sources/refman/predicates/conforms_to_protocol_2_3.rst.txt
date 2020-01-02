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


.. index:: pair: conforms_to_protocol/2-3; Built-in predicate
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

Modes and number of proofs
--------------------------

::

   conforms_to_protocol(?object_identifier, ?protocol_identifier) - zero_or_more
   conforms_to_protocol(?category_identifier, ?protocol_identifier) - zero_or_more

   conforms_to_protocol(?object_identifier, ?protocol_identifier, ?scope) - zero_or_more
   conforms_to_protocol(?category_identifier, ?protocol_identifier, ?scope) - zero_or_more

Errors
------

| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Category`` is neither a variable nor a valid category identifier:
|     ``type_error(category_identifier, Category)``
| ``Protocol`` is neither a variable nor a valid protocol identifier:
|     ``type_error(protocol_identifier, Protocol)``
| ``Scope`` is neither a variable nor an atom:
|     ``type_error(atom, Scope)``
| ``Scope`` is an atom but an invalid entity scope:
|     ``domain_error(scope, Scope)``

Examples
--------

::

   % enumerate objects and categories that conform to the listp protocol:
   | ?- conforms_to_protocol(Object, listp).

   % enumerate objects and categories that privately conform to the listp protocol:
   | ?- conforms_to_protocol(Object, listp, private).

   % enumerate only objects that conform to the listp protocol:
   | ?- current_object(Object), conforms_to_protocol(Object, listp).

   % enumerate only categories that conform to the serialization protocol:
   | ?- current_category(Category), conforms_to_protocol(Category, serialization).

.. seealso::

   :ref:`predicates_current_object_1`,
   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_current_category_1`,
   :ref:`predicates_implements_protocol_2_3`
