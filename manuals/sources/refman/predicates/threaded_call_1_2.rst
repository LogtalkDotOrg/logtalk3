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


.. index:: threaded_call/1-2
.. _predicates_threaded_call_1_2:

threaded_call/1-2
=================

Description
-----------

::

   threaded_call(Goal)
   threaded_call(Goal, Tag)

Proves ``Goal`` asynchronously using a new thread. The argument can be a
message sending goal. Calls to this predicate always succeeds and return
immediately. The results (success, failure, or exception) are sent back
to the message queue of the object containing the call
(:term:`this`); they can be retrieved by calling
the :ref:`predicates_threaded_exit_1_2` predicate.

The variant ``threaded_call/2`` returns a threaded call identifier tag
that can be used with the :ref:`predicates_threaded_exit_1_2` predicate.
Tags shall be regarded as opaque terms; users shall not rely on its type.

Modes and number of proofs
--------------------------

::

   threaded_call(@callable) - one
   threaded_call(@callable, --nonvar) - one

Errors
------

| Goal is a variable:
|     ``instantiation_error``
| Goal is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``
| Tag is not a variable:
|     ``type_error(variable, Goal)``

Examples
--------

| Prove ``Goal`` asynchronously in a new thread:
|     ``threaded_call(Goal)``
| Prove ``::Message`` asynchronously in a new thread:
|     ``threaded_call(::Message)``
| Prove ``Object::Message`` asynchronously in a new thread:
|     ``threaded_call(Object::Message)``

.. seealso::

   :ref:`predicates_threaded_exit_1_2`,
   :ref:`predicates_threaded_ignore_1`,
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_peek_1_2`,
   :ref:`predicates_threaded_1`,
   :ref:`directives_synchronized_1`
