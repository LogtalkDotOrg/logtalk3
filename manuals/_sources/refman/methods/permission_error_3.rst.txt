..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: permission_error/3; Built-in method
.. _methods_permission_error_3:

``permission_error/3``
======================

Description
-----------

::

   permission_error(Operation, Permission, Culprit)

Throws an permission error. Used when an operation is not allowed. For example,
sending a message for a predicate that is not within the scope of the sender.
This built-in method is declared private and thus cannot be used as a message
to an object. Calling this predicate is equivalent to the following sequence
of goals:

::

   ...,
   context(Context),
   throw(error(permission_error(Operation,Permission,Culprit), Context)).

This allows the user to generate errors in the same format used by the
runtime.

Possible values for ``Operation`` include:

- ``access``
- ``create``
- ``modify``
- ``open``
- ``input``
- ``output``
- ``reposition``
- ``repeat``

Possible values for ``Permission`` include:

- ``predicate_declaration``
- ``protected_predicate``
- ``private_predicate``
- ``static_predicate``
- ``database``
- ``object``
- ``static_object``
- ``static_protocol``
- ``static_category``
- ``entity_relation``
- ``operator``
- ``flag``
- ``engine``
- ``binary_stream``
- ``text_stream``
- ``source_sink``
- ``stream``
- ``past_end_of_stream``

The value of ``Culprit`` is the argument or one of its sub-terms that caused
the error.

Modes and number of proofs
--------------------------

::

   permission_error(@nonvar, @nonvar, @nonvar) - error

Errors
------

| When called:
|     ``permission_error(Operation, Permission, Culprit)``

Examples
--------

::

   ...,
   \+ writable(File),
   permission_error(modify, file, File).

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_uninstantiation_error_1`,
   :ref:`methods_type_error_2`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_representation_error_1`,
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
