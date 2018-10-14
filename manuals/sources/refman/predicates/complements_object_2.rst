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


.. index:: complements_object/2
.. _predicates_complements_object_2:

complements_object/2
====================

Description
-----------

::

   complements_object(Category, Object)

Enumerates, by backtracking, all category–object pairs such that the
category explicitly complements the object.

Modes and number of proofs
--------------------------

::

   complements_object(?category_identifier, ?object_identifier) - zero_or_more

Errors
------

Category is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Prototype)``
Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Parent)``

Examples
--------

::

   | ?- complements_object(logging, employee).

.. seealso::

   :ref:`predicates_current_category_1`,
   :ref:`predicates_imports_category_2_3`
