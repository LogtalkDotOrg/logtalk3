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

.. index:: pair: catch/3; Built-in method
.. _methods_catch_3:

``catch/3``
===========

Description
-----------

::

   catch(Goal, Catcher, Recovery)

Catches exceptions thrown by a goal. See also the ISO Prolog standard
definition. This built-in meta-predicate is declared as a private method
and thus cannot be used as a message to an object.

Modes and number of proofs
--------------------------

::

   catch(?callable, ?term, ?callable) - zero_or_more

Errors
------

(none)

Examples
--------

(none)

.. seealso::

   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_uninstantiation_error_1`,
   :ref:`methods_type_error_2`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_representation_error_1`
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
