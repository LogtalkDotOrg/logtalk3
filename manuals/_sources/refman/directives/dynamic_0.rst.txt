..
   This file is part of Logtalk <https://logtalk.org/>  
   SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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

**directive**

.. index:: pair: dynamic/0; Directive
.. _directives_dynamic_0:

``dynamic/0``
=============

Description
-----------

::

   dynamic

Declares an entity and its contents as dynamic. Dynamic entities can be
abolished at runtime.

As entities created at runtime (using the :ref:`predicates_create_object_4`,
:ref:`predicates_create_protocol_3`, and :ref:`predicates_create_category_4`
built-in predicates) are always dynamic, this directive is only necessary in
the rare cases where we want to define dynamic entities in source files.

.. warning::

   Some backend Prolog compilers declare the atom ``dynamic`` as an operator
   for a lighter syntax, forcing writing this atom between parenthesis
   when using this directive.

Template and modes
------------------

::

   dynamic

Examples
--------

::

   :- dynamic.

.. seealso::

   :ref:`directives_dynamic_1`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_protocol_property_2`,
   :ref:`predicates_category_property_2`
