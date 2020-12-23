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


.. index:: pair: ^^/1; Control construct
.. _control_call_super_1:

``^^/1``
========

Description
-----------

::

   ^^Predicate

Calls an imported or inherited predicate definition. The call fails if
the predicate is declared but there is no imported or inherited
predicate definition (as per the :term:`closed-world assumption`). This
control construct may be used within objects or categories in the body
of a predicate definition.

This control construct preserves the implicit execution context
:term:`self` and :term:`sender` arguments (plus the meta-call
context and coinduction stack when applicable) when calling the
inherited (or imported) predicate definition.

The lookups for the predicate declaration and the predicate definition
are performed using a depth-first strategy. Depending on the value of
the :ref:`optimize <flag_optimize>` flag, these lookups are performed
at compile time when the predicate is static and sufficient information
is available. When the lookups are performed at runtime, a caching
mechanism is used to improve performance in subsequent calls. See the
User Manual section on :ref:`performance <performance_performance>` for
details.

When the call is made from within an object, the lookup for the
predicate definition starts at the imported categories, if any. If an
imported predicate definition is not found, the lookup proceeds to the
ancestor objects. Calls from predicates defined in complementing
categories lookup inherited definitions as if the calls were made from
the complemented object, thus allowing more comprehensive object
patching. For other categories, the predicate definition lookup is
restricted to the extended categories.

The called predicate should be declared :ref:`public <directives_public_1>`
or :ref:`protected <directives_protected_1>`. It may also be declared
:ref:`private <directives_private_1>` if within the scope of the entity
where the method making the call is defined.

This control construct is a generalization of the Smalltalk *super*
keyword to take into account Logtalk support for prototypes and
categories besides classes.

Modes and number of proofs
--------------------------

::

   ^^+callable - zero_or_more

Errors
------

| ``Predicate`` is a variable:
|     ``instantiation_error``
| ``Predicate`` is neither a variable nor a callable term:
|     ``type_error(callable, Predicate)``
| ``Predicate``, with predicate indicator ``Name/Arity``, is declared private:
|     ``permission_error(access, private_predicate, Name/Arity)``
| ``Predicate``, with predicate indicator ``Name/Arity``, is not declared:
|     ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

::

   % specialize the inherited definition
   % of the init/0 predicate:
   init :-
       assertz(counter(0)),
       ^^init.

.. seealso::

   :ref:`control_send_to_object_2`,
   :ref:`control_send_to_self_1`,
   :ref:`control_delegate_message_1`
