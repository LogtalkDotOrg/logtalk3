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


.. index:: instantiation_error/0
.. _methods_instantiation_error_0:

instantiation_error/0
=====================

Description
-----------

::

   instantiation_error

Throws an instantiation error. This built-in predicate is declared as
a private method and thus cannot be used as a message to an object.
Calling this predicate is equivalent to the following sequence of calls:

::

   ...,
   context(Context),
   throw(error(instantiation_error, Context)).

This allows the user to generate errors in the same format used by the
runtime.

Modes and number of proofs
--------------------------

::

   instantiation_error - error

Errors
------

| When called:
|     ``instantiation_error``

Examples
--------

::

   ...,
   var(Handler),
   instantiation_error.

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_type_error_2`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_representation_error_1`,
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
