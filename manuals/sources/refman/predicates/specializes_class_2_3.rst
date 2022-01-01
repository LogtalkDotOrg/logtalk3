..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: specializes_class/2-3; Built-in predicate
.. _predicates_specializes_class_2_3:

``specializes_class/2-3``
=========================

Description
-----------

::

   specializes_class(Class, Superclass)
   specializes_class(Class, Superclass, Scope)

Enumerates, by backtracking, all pairs of objects such that the first
one specializes the second. The relation scope is represented by the
atoms ``public``, ``protected``, and ``private``.

Modes and number of proofs
--------------------------

::

   specializes_class(?object_identifier, ?object_identifier) - zero_or_more
   specializes_class(?object_identifier, ?object_identifier, ?scope) - zero_or_more

Errors
------

| ``Class`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Class)``
| ``Superclass`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Superclass)``
| ``Scope`` is neither a variable nor an atom:
|     ``type_error(atom, Scope)``
| ``Scope`` is an atom but an invalid entity scope:
|     ``domain_error(scope, Scope)``

Examples
--------

::

   % enumerate the state_space subclasses:
   | ?- specializes_class(Subclass, state_space).

   % enumerate the state_space subclasses where the
   % specialization relation is public:
   | ?- specializes_class(Subclass, state_space, public).

.. seealso::

   :ref:`predicates_current_object_1`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`
