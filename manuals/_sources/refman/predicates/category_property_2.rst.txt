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


.. rst-class:: align-right

**built-in predicate**

.. index:: pair: category_property/2; Built-in predicate
.. _predicates_category_property_2:

``category_property/2``
=======================

Description
-----------

::

   category_property(Category, Property)

Enumerates, by backtracking, the properties associated with the defined
categories. The valid properties are listed in the language grammar section
on :ref:`entity properties <grammar_entity_properties>` and described in
the User Manual section on :ref:`category properties <categories_properties>`.

Modes and number of proofs
--------------------------

::

   category_property(?category_identifier, ?category_property) - zero_or_more

Errors
------

| ``Category`` is neither a variable nor a valid category identifier:
|     ``type_error(category_identifier, Category)``
|
| ``Property`` is neither a variable nor a callable term:
|     ``type_error(callable, Property)``
| ``Property`` is a callable term but not a valid category property:
|     ``domain_error(category_property, Property)``

Examples
--------

::

   % enumerate the properties of the core_messages built-in category:
   | ?- category_property(core_messages, Property).

   Property = source_data ;
   Property = static ;
   Property = built_in ;
   ...

.. seealso::

   :ref:`predicates_abolish_category_1`,
   :ref:`predicates_create_category_4`,
   :ref:`predicates_current_category_1`,
   :ref:`predicates_complements_object_2`,
   :ref:`predicates_extends_category_2_3`,
   :ref:`predicates_imports_category_2_3`
