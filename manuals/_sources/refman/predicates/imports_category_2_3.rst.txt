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


.. index:: pair: imports_category/2-3; Built-in predicate
.. _predicates_imports_category_2_3:

``imports_category/2-3``
========================

Description
-----------

::

   imports_category(Object, Category)

   imports_category(Object, Category, Scope)

Enumerates, by backtracking, importation relations between objects and
categories. The relation scope is represented by the atoms ``public``,
``protected``, and ``private``.

Modes and number of proofs
--------------------------

::

   imports_category(?object_identifier, ?category_identifier) - zero_or_more
   imports_category(?object_identifier, ?category_identifier, ?scope) - zero_or_more

Errors
------

| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Category`` is neither a variable nor a valid category identifier:
|     ``type_error(category_identifier, Category)``
| ``Scope`` is neither a variable nor an atom:
|     ``type_error(atom, Scope)``
| ``Scope`` is an atom but an invalid entity scope:
|     ``domain_error(scope, Scope)``

Examples
--------

::

   % check that the xref_diagram object imports the diagram category:
   | ?- imports_category(xref_diagram, diagram).

   % enumerate the objects that privately import the diagram category:
   | ?- imports_category(Object, diagram, private).

.. seealso::

   :ref:`predicates_current_category_1`,
   :ref:`predicates_complements_object_2`
