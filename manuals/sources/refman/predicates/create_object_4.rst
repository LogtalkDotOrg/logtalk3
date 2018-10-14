..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: create_object/4
.. _predicates_create_object_4:

create_object/4
===============

Description
-----------

::

   create_object(Identifier, Relations, Directives, Clauses)

Creates a new, dynamic object. The word :term:`object` is used here
as a generic term. This predicate can be used to create new prototypes,
instances, and classes. This predicate is often used as a primitive to
implement high-level object creation methods.

Note that, when opting for runtime generated object identifiers, it's
possible to run out of identifiers when using a back-end Prolog compiler
with bounded integer support. The portable solution, when creating a
large number of dynamic objects in long-running applications, is to
recycle, whenever possible, the identifiers.

When using Logtalk multi-threading features, predicates calling this
built-in predicate may need to be declared synchronized in order to
avoid race conditions.

Modes and number of proofs
--------------------------

::

   create_object(?object_identifier, @list(object_relation), @list(object_directive), @list(clause)) - one

Errors
------

Relations, Directives, or Clauses is a variable:
   ``instantiation_error``
Identifier is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Identifier)``
Identifier is already in use:
   ``permission_error(modify, category, Identifier)``
   ``permission_error(modify, object, Identifier)``
   ``permission_error(modify, protocol, Identifier)``
Relations is neither a variable nor a proper list:
   ``type_error(list, Relations)``
Repeated entity relation clause:
   ``permission_error(repeat, entity_relation, implements/1)``
   ``permission_error(repeat, entity_relation, imports/1)``
   ``permission_error(repeat, entity_relation, extends/1)``
   ``permission_error(repeat, entity_relation, instantiates/1)``
   ``permission_error(repeat, entity_relation, specializes/1)``
Directives is neither a variable nor a proper list:
   ``type_error(list, Directives)``
Clauses is neither a variable nor a proper list:
   ``type_error(list, Clauses)``

Examples
--------

Creating a simple, stand-alone object (a prototype):
   ``| ?- create_object(translator, [], [public(int/2)], [int(0, zero)]).``
Creating a new prototype derived from a parent prototype:
   ``| ?- create_object(mickey, [extends(mouse)], [public(alias/1)], [alias(mortimer)]).``
Creating a new class instance:
   ``| ?- create_object(p1, [instantiates(person)], [], [name('Paulo Moura'), age(42)]).``
Creating a new class as a specialization of another class:
   ``| ?- create_object(hovercraft, [specializes(vehicle)], [public([propeller/2, fan/2])], []).``

Creating a new object and defining its initialization goal:
   ``| ?- create_object(runner, [instantiates(runners)], [initialization(start)], [length(22), time(60)]).``

Creating a new empty object with dynamic predicate declarations support:
   ``| ?- create_object(database, [], [set_logtalk_flag(dynamic_declarations, allow)], []).``

.. seealso::

   :ref:`predicates_abolish_object_1`,
   :ref:`predicates_current_object_1`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`,
   :ref:`predicates_complements_object_2`
