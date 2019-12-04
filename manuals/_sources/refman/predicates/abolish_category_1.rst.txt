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


.. index:: pair: abolish_category/1; Built-in predicate
.. _predicates_abolish_category_1:

abolish_category/1
==================

Description
-----------

::

   abolish_category(Category)

Abolishes a dynamic category. The category identifier can then be reused when creating a new category.

Modes and number of proofs
--------------------------

::

   abolish_category(+category_identifier) - one

Errors
------

| ``Category`` is a variable:
|     ``instantiation_error``
| ``Category`` is neither a variable nor a valid category identifier:
|     ``type_error(category_identifier, Category)``
| ``Category`` is an identifier of a static category:
|     ``permission_error(modify, static_category, Category)``
| ``Category`` does not exist:
|     ``existence_error(category, Category)``

Examples
--------

::

   | ?- abolish_category(monitoring).

.. seealso::

   :ref:`predicates_category_property_2`,
   :ref:`predicates_create_category_4`,
   :ref:`predicates_current_category_1`
   :ref:`predicates_complements_object_2`,
   :ref:`predicates_extends_category_2_3`,
   :ref:`predicates_imports_category_2_3`
