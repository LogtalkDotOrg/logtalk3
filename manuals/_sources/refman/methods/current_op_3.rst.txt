..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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

**built-in method**

.. index:: pair: current_op/3; Built-in method
.. _methods_current_op_3:

``current_op/3``
================

Description
-----------

::

   current_op(Priority, Specifier, Operator)

Enumerates, by backtracking, the visible operators declared for an
object. Operators not declared using a scope directive are not
enumerated.

Modes and number of proofs
--------------------------

::

   current_op(?operator_priority, ?operator_specifier, ?atom) - zero_or_more

Errors
------

| ``Priority`` is neither a variable nor an integer:
|     ``type_error(integer, Priority)``
| ``Priority`` is an integer but not a valid operator priority:
|     ``domain_error(operator_priority, Priority)``
| ``Specifier`` is neither a variable nor an atom:
|     ``type_error(atom, Specifier)``
| ``Specifier`` is an atom but not a valid operator specifier:
|     ``domain_error(operator_specifier, Specifier)``
| ``Operator`` is neither a variable nor an atom:
|     ``type_error(atom, Operator)``

Examples
--------

| To enumerate, by backtracking, the local operators or the operators visible in :term:`this`:
|     ``current_op(Priority, Specifier, Operator)``
| To enumerate, by backtracking, the public and protected operators visible in :term:`self`:
|     ``::current_op(Priority, Specifier, Operator)``
| To enumerate, by backtracking, the public operators visible for an explicit object:
|     ``Object::current_op(Priority, Specifier, Operator)``

.. seealso::

   :ref:`methods_current_predicate_1`,
   :ref:`methods_predicate_property_2`,
   :ref:`directives_op_3`
