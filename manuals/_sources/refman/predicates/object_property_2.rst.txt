..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: object_property/2; Built-in predicate
.. _predicates_object_property_2:

object_property/2
=================

Description
-----------

::

   object_property(Object, Property)

Enumerates, by backtracking, the properties associated with the defined
objects. The valid properties are listed in the language grammar section
on :ref:`entity properties <grammar_entity_properties>` and described in
the User Manual section on :ref:`object properties <objects_properties>`.

Modes and number of proofs
--------------------------

::

   object_property(?object_identifier, ?object_property) - zero_or_more

Errors
------

| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Property`` is neither a variable nor a callable term:
|     ``type_error(callable, Property)``
| ``Property`` is a callable term but not a valid object property:
|     ``domain_error(object_property, Property)``

Examples
--------

::

   % enumerate the properties of the logtalk built-in object:
   | ?- object_property(logtalk, Property).

   Property = context_switching_calls ;
   Property = source_data ;
   Property = threaded ;
   Property = static ;
   Property = built_in ;
   ...

.. seealso::

   :ref:`predicates_abolish_object_1`,
   :ref:`predicates_create_object_4`,
   :ref:`predicates_current_object_1`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`,
   :ref:`predicates_complements_object_2`
