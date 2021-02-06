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


.. index:: pair: ::/2; Control construct
.. _control_send_to_object_2:

``::/2``
========

Description
-----------

::

   Object::Message
   {Proxy}::Message

Sends a message to an object. The message argument must match a
:ref:`public <directives_public_1>` predicate of the receiver object. When
the message corresponds to a :ref:`protected <directives_protected_1>` or
:ref:`private <directives_private_1>` predicate, the call is only valid if
the :term:`sender` matches the :term:`predicate scope container`. When the
predicate is declared but not defined, the message simply fails (as per
the :term:`closed-world assumption`).

The ``{Proxy}::Message`` syntax allows simplified access to
:term:`parametric object proxies <parametric object proxy>`.
Its operational semantics is equivalent to the conjunction
``(call(Proxy), Proxy::Message)``. I.e. ``Proxy`` is proved
within the context of the pseudo-object :ref:`user <objects_user>` and,
if successful, the ``Proxy`` term is used as an object identifier.
Exceptions thrown when proving ``Proxy`` are handled by the ``::/2``
control construct. This construct construct supports backtracking over
the ``{Proxy}`` goal.

The lookups for the message declaration and the corresponding method are
performed using a depth-first strategy. Depending on the value of the
:ref:`optimize <flag_optimize>` flag, these lookups are performed at
compile time whenever sufficient information is available. When the
lookups are performed at runtime, a caching mechanism is used to improve
performance in subsequent messages. See the User Manual section on
:ref:`performance <performance_performance>` for details.

Modes and number of proofs
--------------------------

::

   +object_identifier::+callable - zero_or_more
   {+object_identifier}::+callable - zero_or_more

Errors
------

| Either ``Object`` or ``Message`` is a variable:
|     ``instantiation_error``
| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Message`` is neither a variable nor a callable term:
|     ``type_error(callable, Message)``
| ``Message``, with predicate indicator ``Name/Arity``, is declared private:
|     ``permission_error(access, private_predicate, Name/Arity)``
| ``Message``, with predicate indicator ``Name/Arity``, is declared protected:
|     ``permission_error(access, protected_predicate, Name/Arity)``
| ``Message``, with predicate indicator ``Name/Arity``, is not declared:
|     ``existence_error(predicate_declaration, Name/Arity)``
| ``Object`` does not exist:
|     ``existence_error(object, Object)``
| 
| ``Proxy`` is a variable:
|     ``instantiation_error``
| ``Proxy`` is neither a variable nor a callable term:
|     ``type_error(callable, Proxy)``
| ``Proxy``, with predicate indicator ``Name/Arity``, does not exist in the ``user`` pseudo-object:
|     ``existence_error(procedure, Name/Arity)``

Examples
--------

::

   | ?- list::member(X, [1, 2, 3]).

   X = 1 ;
   X = 2 ;
   X = 3
   yes

.. seealso::

   :ref:`control_send_to_self_1`,
   :ref:`control_call_super_1`,
   :ref:`control_delegate_message_1`
