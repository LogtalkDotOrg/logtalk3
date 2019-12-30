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


.. index:: pair: call//1-N; Built-in method
.. _methods_call_1:

call//1-N
=========

Description
-----------

::

   call(Closure)
   call(Closure, Arg1, ...)
   call(Object::Closure, Arg1, ...)
   call(::Closure, Arg1, ...)
   call(^^Closure, Arg1, ...)
   ...

This non-terminal takes a :term:`closure` and is processed by appending the
input list of tokens and the list of remaining tokens to the arguments
of the closure. This built-in non-terminal is interpreted as a private
non-terminal and thus cannot be used as a message to an object.

Using this non-terminal is recommended when calling a predicate whose
last two arguments are the input list of tokens and the list of remaining
tokens to avoid hard-coding assumptions about how grammar rules are
compiled into clauses. Note that the compiler ensures zero overhead when
using this non-terminal with a bound argument at compile time.

When using a :term:`backend Prolog compiler` supporting a module system,
calls in the format ``call(Module:Closure)`` may also be used. By using
as argument a :term:`lambda expression`, this built-in non-terminal can
provide controlled access to the input list of tokens and to the list of
the remaining tokens processed by the grammar rule containing the call.

Modes and number of proofs
--------------------------

::

   call(+callable) - zero_or_more
   call(+callable, ?term) - zero_or_more
   call(+callable, ?term, ?term) - zero_or_more
   ...

Errors
------

| ``Closure`` is a variable:
|     ``instantiation_error``
| ``Closure`` is neither a variable nor a callable term:
|     ``type_error(callable, Closure)``

Examples
--------

| Calls a goal, constructed by appending the tokens difference list to the closure, in in the context of the object or category containing the call:
|     ``call(Closure)``
| To make a *super* call, constructed by appending the tokens difference list to the closure:
|     ``call(^^Closure)``
| To send a goal, constructed by appending the tokens difference list to the closure, as a message to :term:`self`:
|     ``call(::Closure)``
| To send a goal, constructed by appending the tokens difference list to the closure, as a message to an explicit object:
|     ``call(Object::Closure)``

.. seealso::

   :ref:`methods_eos_0`,
   :ref:`methods_phrase_1`,
   :ref:`methods_phrase_2`,
   :ref:`methods_phrase_3`
