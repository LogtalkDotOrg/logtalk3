..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
