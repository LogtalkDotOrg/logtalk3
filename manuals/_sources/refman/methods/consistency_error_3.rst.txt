..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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

.. index:: pair: consistency_error/3; Built-in method
.. _methods_consistency_error_3:

``consistency_error/3``
=======================

Description
-----------

::

   consistency_error(Consistency, Argument1, Argument2)

Throws a consistency error. Used when two directives or predicate arguments are
individually correct but together are not consistent. For example, a predicate
and its alias having different arity in a ``uses/2`` directive. This built-in
method is declared private and thus cannot be used as a message to an object.
Calling this predicate is equivalent to the following sequence of goals:

::

   ...,
   context(Context),
   throw(error(consistency_error(Consistency,Argument1,Argument2), Context)).

This allows the user to generate errors in the same format used by the
runtime.

Possible values representing ``Consistency`` checks include:

- ``same_arity``
- ``same_number_of_parameters``
- ``same_number_of_arguments``
- ``same_closure_specification``

Modes and number of proofs
--------------------------

::

   consistency_error(+atom, @nonvar, @nonvar) - error

Errors
------

| When called:
|     ``consistency_error(Consistency, Argument1, Argument2)``

Examples
--------

::

   % code that will trigger consistency errors when compiled:

   % predicates (and non-terminals) aliases must have the same
   % arity as the original predicates (and non-terminals)
   :- uses(list, [
       member/2 as in/1
   ]).

   % meta-predicate templates should be consistent with how closures
   % are used regarding the number of additional arguments
   :- public(p/2).
   :- meta_predicate(p(1, *)).

   p(G, A) :-
       call(G, A, 2).

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_uninstantiation_error_1`,
   :ref:`methods_type_error_2`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_representation_error_1`,
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
