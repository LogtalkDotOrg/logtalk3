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

   extends_category(Category1, Category2)
   extends_category(Category1, Category2, Scope)

Enumerates, by backtracking, all pairs of categories such that the first
one extends the second. The relation scope is represented by the atoms
``public``, ``protected``, and ``private``.

Template and modes
------------------

::

   extends_category(?category_identifier, ?category_identifier)
   extends_category(?category_identifier, ?category_identifier, ?scope)

Errors
------

Category1 is neither a variable nor a valid protocol identifier:
   ``type_error(category_identifier, Category1)``
Category2 is neither a variable nor a valid protocol identifier:
   ``type_error(category_identifier, Category2)``
Scope is neither a variable nor an atom:
   ``type_error(atom, Scope)``
Scope is an atom but an invalid entity scope:
   ``domain_error(scope, Scope)``

Examples
--------

::

   | ?- extends_category(basic, Category).

   | ?- extends_category(Category, extended, private).

.. seealso::

   :ref:`predicates_current_category_1`,
   :ref:`predicates_complements_object_2`,
   :ref:`predicates_imports_category_2_3`
