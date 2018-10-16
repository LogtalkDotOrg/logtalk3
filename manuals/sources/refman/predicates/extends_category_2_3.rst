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


.. index:: extends_category/2-3
.. _predicates_extends_category_2_3:

extends_category/2-3
====================

Description
-----------

::

   extends_category(Category, ParentCategory)
   extends_category(Category, ParentCategory, Scope)

Enumerates, by backtracking, all pairs of categories such that the first
one extends the second. The relation scope is represented by the atoms
``public``, ``protected``, and ``private``.

Modes and number of proofs
--------------------------

::

   extends_category(?category_identifier, ?category_identifier) - zero_or_more
   extends_category(?category_identifier, ?category_identifier, ?scope) - zero_or_more

Errors
------

| Category is neither a variable nor a valid protocol identifier:
|     ``type_error(category_identifier, Category)``
| ParentCategory is neither a variable nor a valid protocol identifier:
|     ``type_error(category_identifier, ParentCategory)``
| Scope is neither a variable nor an atom:
|     ``type_error(atom, Scope)``
| Scope is an atom but an invalid entity scope:
|     ``domain_error(scope, Scope)``

Examples
--------

::

   % enumerate the categories extended by the derailleur category:
   | ?- extends_category(derailleur, Category).

   % enumerate categories that privately extend the basics category:
   | ?- extends_category(Category, basics, private).

.. seealso::

   :ref:`predicates_current_category_1`,
   :ref:`predicates_complements_object_2`,
   :ref:`predicates_imports_category_2_3`
