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

.. index:: pair: domain_error/2; Built-in method
.. _methods_domain_error_2:

``domain_error/2``
==================

Description
-----------

::

   domain_error(Domain, Culprit)

Throws a domain error. Used when an argument is of the correct type but
outside the valid domain. For example, trying to use an atom as an operator
specifier that is not a valid specifier. This built-in method is declared
private and thus cannot be used as a message to an object. Calling this
predicate is equivalent to the following sequence of goals:

::

   ...,
   context(Context),
   throw(error(domain_error(Domain,Culprit), Context)).

This allows the user to generate errors in the same format used by the
runtime.

Possible values for ``Domain`` include:

- ``character_code_list``
- ``close_option``
- ``flag_option``
- ``flag_value``
- ``compiler_flag``
- ``flag``
- ``prolog_flag``
- ``io_mode``
- ``non_empty_list``
- ``not_less_than_zero``
- ``operator_priority``
- ``operator_specifier``
- ``read_option``
- ``source_sink``
- ``stream``
- ``stream_option``
- ``stream_or_alias``
- ``stream_position``
- ``stream_property``
- ``write_option``
- ``character_code_list``
- ``text_encoding``
- ``directive``
- ``object_directive``
- ``protocol_directive``
- ``category_directive``
- ``object_relation``
- ``protocol_relation``
- ``category_relation``
- ``object_property``
- ``protocol_property``
- ``category_property``
- ``predicate_property``
- ``meta_argument_specifier``
- ``meta_directive_template``
- ``closure``
- ``allocation``
- ``redefinition``
- ``message_sending_goal``
- ``class``
- ``prototype``
- ``scope``
- ``boolean``

The value of ``Culprit`` is the argument or one of its sub-terms that caused
the error.

Modes and number of proofs
--------------------------

::

   domain_error(+atom, @nonvar) - error

Errors
------

| When called:
|     ``domain_error(Domain, Culprit)``

Examples
--------

::

   ...,
   atom(Color),
   \+ color(Color),
   domain_error(color, Color).

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_uninstantiation_error_1`,
   :ref:`methods_type_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_representation_error_1`,
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
