..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: abolish_object/1; Built-in predicate
.. _predicates_abolish_object_1:

``abolish_object/1``
====================

Description
-----------

::

   abolish_object(Object)

Abolishes a dynamic object. The object identifier can then be reused when creating a new object.

Modes and number of proofs
--------------------------

::

   abolish_object(+object_identifier) - one

Errors
------

| ``Object`` is a variable:
|     ``instantiation_error``
| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Object`` is an identifier of a static object:
|     ``permission_error(modify, static_object, Object)``
| ``Object`` does not exist:
|     ``existence_error(object, Object)``

Examples
--------

::

   | ?- abolish_object(list).

.. seealso::

   :ref:`predicates_create_object_4`,
   :ref:`predicates_current_object_1`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`,
   :ref:`predicates_complements_object_2`
