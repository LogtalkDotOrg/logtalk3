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


.. index:: pair: context/1; Built-in method
.. _methods_context_1:

context/1
=========

Description
-----------

::

   context(Context)

Returns the execution context for a predicate clause using the term
``logtalk(Head,ExecutionContext)`` where ``Head`` is the head of the
clause containing the call. This private predicate is mainly used for
providing a default error context when type-checking predicate
arguments. The ``ExecutionContext`` term should be regarded as an opaque
term, which can be decoded using the
:ref:`logtalk::execution_context/7 <apis:logtalk/0::execution_context/7>`
predicate. Calls to this predicate are inlined at compilation time.

Modes and number of proofs
--------------------------

::

   context(--callable) - one

Errors
------

| Context is not a variable:
|     ``type_error(var, Context)``

Examples
--------

::

   foo(A, N) :-
       % type-check arguments
       context(Context),
       type::check(atom, A, Context),
       type::check(integer, N, Context),
       % arguments are fine; go ahead
       ... .

.. seealso::

   :ref:`methods_parameter_2`,
   :ref:`methods_self_1`,
   :ref:`methods_sender_1`,
   :ref:`methods_this_1`
