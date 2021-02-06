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


.. index:: pair: threaded_ignore/1; Built-in predicate
.. _predicates_threaded_ignore_1:

``threaded_ignore/1``
=====================

Description
-----------

::

   threaded_ignore(Goal)

Proves ``Goal`` asynchronously using a new thread. Only the first goal
solution is found. The argument can be a message sending goal. This call
always succeeds, independently of the result (success, failure, or
exception), which is simply discarded instead of being sent back to the
message queue of the object containing the call
(:term:`this`).

Modes and number of proofs
--------------------------

::

   threaded_ignore(@callable) - one

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

| Prove ``Goal`` asynchronously in a new thread:
|     ``threaded_ignore(Goal)``
| Prove ``::Message`` asynchronously in a new thread:
|     ``threaded_ignore(::Message)``
| Prove ``Object::Message`` asynchronously in a new thread:
|     ``threaded_ignore(Object::Message)``

.. seealso::

   :ref:`predicates_threaded_call_1_2`,
   :ref:`predicates_threaded_exit_1_2`,
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_peek_1_2`,
   :ref:`predicates_threaded_1`,
   :ref:`directives_synchronized_1`
