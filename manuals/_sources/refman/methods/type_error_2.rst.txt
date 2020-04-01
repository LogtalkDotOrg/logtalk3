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


.. index:: pair: type_error/2; Built-in method
.. _methods_type_error_2:

type_error/2
============

Description
-----------

::

   type_error(Type, Culprit)

Throws a type error. Used when the type of an argument is incorrect. For example,
trying ot use a non-callable term as a message. This built-in method is declared
private and thus cannot be used as a message to an object. Calling this predicate
is equivalent to the following sequence of goals:

::

   ...,
   context(Context),
   throw(error(type_error(Type,Culprit), Context)).

This allows the user to generate errors in the same format used by the
runtime.

Possible values for ``Type`` include all the types defined by the ``type``
library object and by other libraries such as ``os``, ``expecteds``, and
``optionals``. The value of ``Culprit`` is the argument or one of its sub-terms 
that caused the error.

Modes and number of proofs
--------------------------

::

   type_error(@nonvar, @term) - error

Errors
------

| When called:
|     ``type_error(Type, Culprit)``

Examples
--------

::

   ...,
   \+ atom(Name),
   type_error(atom, Name).

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_representation_error_1`
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`,
