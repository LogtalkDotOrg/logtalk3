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


.. index:: pair: instantiates_class/2-3; Built-in predicate
.. _predicates_instantiates_class_2_3:

``instantiates_class/2-3``
==========================

Description
-----------

::

   instantiates_class(Instance, Class)
   instantiates_class(Instance, Class, Scope)

Enumerates, by backtracking, all pairs of objects such that the first
one instantiates the second. The relation scope is represented by the
atoms ``public``, ``protected``, and ``private``.

Modes and number of proofs
--------------------------

::

   instantiates_class(?object_identifier, ?object_identifier) - zero_or_more
   instantiates_class(?object_identifier, ?object_identifier, ?scope) - zero_or_more

Errors
------

| ``Instance`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Instance)``
| ``Class`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Class)``
| ``Scope`` is neither a variable nor an atom:
|     ``type_error(atom, Scope)``
| ``Scope`` is an atom but an invalid entity scope:
|     ``domain_error(scope, Scope)``

Examples
--------

::

   % check that the water_jug is an instante of state_space:
   | ?- instantiates_class(water_jug, state_space).

   % enumerate the state_space instances where the
   % instantiation relation is public:
   | ?- instantiates_class(Space, state_space, public).

.. seealso::

   :ref:`predicates_current_object_1`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_specializes_class_2_3`
