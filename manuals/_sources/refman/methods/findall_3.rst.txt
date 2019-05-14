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


.. index:: pair: findall/3; Built-in method
.. _methods_findall_3:

findall/3
=========

Description
-----------

::

   findall(Template, Goal, List)

Collects a list of solutions for the goal. The order of the elements in
the list follows the order of the goal solutions. It succeeds returning
an empty list when the goal has no solutions.

This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

Modes and number of proofs
--------------------------

::

   findall(?term, +callable, ?list) - zero_or_one

Errors
------

| Goal is a variable:
|     ``instantiation_error``
| Goal is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``
| Goal is a call to a non-existing predicate:
|     ``existence_error(procedure, Predicate)``

Examples
--------

| To find all solutions in the context of the object or category containing the call:
|     ``findall(Template, Goal, List)``
| To find all solutions of sending a message to :term:`self`:
|     ``findall(Template, ::Message, List)``
| To find all solutions of sending a message to an explicit object:
|     ``findall(Template, Object::Message, List)``

.. seealso::

   :ref:`methods_bagof_3`,
   :ref:`methods_findall_4`,
   :ref:`methods_forall_2`,
   :ref:`methods_setof_3`
