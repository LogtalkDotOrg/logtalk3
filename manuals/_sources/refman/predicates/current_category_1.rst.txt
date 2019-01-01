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


.. index:: current_category/1
.. _predicates_current_category_1:

current_category/1
==================

Description
-----------

::

   current_category(Category)

Enumerates, by backtracking, all currently defined categories. All
categories are found, either static, dynamic, or built-in.

Modes and number of proofs
--------------------------

::

   current_category(?category_identifier) - zero_or_more

Errors
------

| Category is neither a variable nor a valid category identifier:
|     ``type_error(category_identifier, Category)``

Examples
--------

::

   % enumerate the defined categories:
   | ?- current_category(Category).
   
   Category = core_messages ;
   ...

.. seealso::

   :ref:`predicates_abolish_category_1`,
   :ref:`predicates_category_property_2`,
   :ref:`predicates_create_category_4`,
   :ref:`predicates_complements_object_2`,
   :ref:`predicates_extends_category_2_3`,
   :ref:`predicates_imports_category_2_3`
