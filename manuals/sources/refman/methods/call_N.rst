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


.. index:: pair: call/1-N; Built-in method
.. _methods_call_N:

call/1-N
========

Description
-----------

::

   call(Goal)
   call(Closure, Arg1, ...)

Calls a goal, which might be constructed by appending additional
arguments to a closure. The upper limit for ``N`` depends on the upper
limit for the arity of a compound term of the :term:`backend Prolog compiler`.
This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object. The ``Closure`` argument can
also be a lambda expression or a Logtalk control construct. When using a
backend Prolog compiler supporting a module system, calls in the format
``call(Module:Closure, Arg1, ...)`` may also be used.

This meta-predicate is opaque to cuts in its arguments.

Modes and number of proofs
--------------------------

::

   call(+callable) - zero_or_more
   call(+callable, ?term) - zero_or_more
   call(+callable, ?term, ?term) - zero_or_more
   ...

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``
| ``Closure`` is a variable:
|     ``instantiation_error``
| ``Closure`` is neither a variable nor a callable term:
|     ``type_error(callable, Closure)``

Examples
--------

| Call a goal, constructed by appending additional arguments to a closure, in the context of the object or category containing the call:
|     ``call(Closure, Arg1, Arg2, ...)``
| To send a goal, constructed by appending additional arguments to a closure, as a message to :term:`self`:
|     ``call(::Closure, Arg1, Arg2, ...)``
| To send a goal, constructed by appending additional arguments to a closure, as a message to an explicit object:
|     ``call(Object::Closure, Arg1, Arg2, ...)``

.. seealso::

   :ref:`methods_ignore_1`,
   :ref:`methods_once_1`,
   :ref:`methods_not_1`
