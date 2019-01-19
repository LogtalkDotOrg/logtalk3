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


.. index:: create_category/4
.. _predicates_create_category_4:

create_category/4
=================

Description
-----------

::

   create_category(Identifier, Relations, Directives, Clauses)

Creates a new, dynamic category. This predicate is often used as a
primitive to implement high-level category creation methods.

Note that, when opting for runtime generated category identifiers, it's
possible to run out of identifiers when using a :term:`backend Prolog compiler`
with bounded integer support. The portable solution, when creating a
large number of dynamic category in long-running applications, is to
recycle, whenever possible, the identifiers.

When using Logtalk multi-threading features, predicates calling this
built-in predicate may need to be declared synchronized in order to
avoid race conditions.

Modes and number of proofs
--------------------------

::

   create_category(?category_identifier, @list(category_relation), @list(category_directive), @list(clause)) - one

Errors
------

| Relations, Directives, or Clauses is a variable:
|     ``instantiation_error``
| Identifier is neither a variable nor a valid category identifier:
|     ``type_error(category_identifier, Identifier)``
| Identifier is already in use:
|     ``permission_error(modify, category, Identifier)``
|     ``permission_error(modify, object, Identifier)``
|     ``permission_error(modify, protocol, Identifier)``
| Relations is neither a variable nor a proper list:
|     ``type_error(list, Relations)``
| Repeated entity relation clause:
|     ``permission_error(repeat, entity_relation, implements/1)``
|     ``permission_error(repeat, entity_relation, extends/1)``
|     ``permission_error(repeat, entity_relation, complements/1)``
| Directives is neither a variable nor a proper list:
|     ``type_error(list, Directives)``
| Clauses is neither a variable nor a proper list:
|     ``type_error(list, Clauses)``

Examples
--------

::

   | ?- create_category(
           tolerances,
           [implements(comparing)],
           [],
           [epsilon(1e-15), (equal(X, Y) :- epsilon(E), abs(X-Y) =< E)]
        ).

.. seealso::

   :ref:`predicates_abolish_category_1`, :ref:`predicates_category_property_2`,
   :ref:`predicates_current_category_1`,
   :ref:`predicates_complements_object_2`, :ref:`predicates_extends_category_2_3`,
   :ref:`predicates_imports_category_2_3`
