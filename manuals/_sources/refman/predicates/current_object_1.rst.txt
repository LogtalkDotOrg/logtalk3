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


.. index:: pair: current_object/1; Built-in predicate
.. _predicates_current_object_1:

current_object/1
================

Description
-----------

::

   current_object(Object)

Enumerates, by backtracking, all currently defined objects. All objects
are found, either static, dynamic or built-in.

Modes and number of proofs
--------------------------

::

   current_object(?object_identifier) - zero_or_more

Errors
------

| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``

Examples
--------

::

   % enumerate the defined objects:
   | ?- current_object(Object).
   
   Object = user ;
   Object = logtalk ;
   ...

.. seealso::

   :ref:`predicates_abolish_object_1`,
   :ref:`predicates_create_object_4`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`,
   :ref:`predicates_complements_object_2`
