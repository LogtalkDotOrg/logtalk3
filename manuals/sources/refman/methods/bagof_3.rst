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


.. index:: pair: bagof/3; Built-in method
.. _methods_bagof_3:

bagof/3
=======

Description
-----------

::

   bagof(Template, Goal, List)

Collects a bag of solutions for the goal for each set of instantiations
of the free variables in the goal. The order of the elements in the bag
follows the order of the goal solutions. The free variables in the goal
are the variables that occur in the goal but not in the template. Free
variables can be ignored, however, by using the ``^/2`` existential
qualifier. For example, if ``T`` is term containing all the free
variables that we want to ignore, we can write ``T^Goal``. Note that the
term ``T`` can be written as ``V1^V2^...``.

When there are free variables, this method is re-executable on
backtracking. This method fails when there are no solutions, never
returning an empty list.

This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

Modes and number of proofs
--------------------------

::

   bagof(@term, +callable, -list) - zero_or_more

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

| To find a bag of solutions in the context of the object or category containing the call:
|     ``bagof(Template, Goal, List)``
| To find a bag of solutions of sending a message to :term:`self`:
|     ``bagof(Template, ::Message, List)``
| To find a bag of solutions of sending a message to an explicit object:
|     ``bagof(Template, Object::Message, List)``

.. seealso::

   :ref:`methods_findall_3`,
   :ref:`methods_findall_4`,
   :ref:`methods_forall_2`,
   :ref:`methods_setof_3`
