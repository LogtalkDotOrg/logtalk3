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


.. index:: pair: <</2; Control construct
.. _control_context_switch_2:

<</2
====

Description
-----------

::

   Object<<Goal
   {Proxy}<<Goal

Debugging control construct. Calls a goal within the context of the
specified object. The goal is called with the execution context
(:term:`sender`, :term:`this`, and :term:`self`) set to the object.
The goal may need to be written between parenthesis to avoid parsing
errors due to operator conflicts. This control construct should only be
used for debugging or for writing unit tests. This control construct can
only be used for objects compiled with the
:ref:`context_switching_calls <flag_context_switching_calls>` compiler
flag set to ``allow``. Set this compiler flag to ``deny`` to disable
this control construct and thus preventing using it to break encapsulation.

The ``{Proxy}<<Goal`` syntax allows simplified access to
:term:`parametric object proxies <parametric object proxy>`.
Its operational semantics is equivalent to the goal conjunction
``(call(Proxy), Proxy<<Goal)``. I.e. ``Proxy`` is proved within the
context of the pseudo-object :ref:`user <objects_user>` and, if successful,
the goal term is used as a parametric object identifier. Exceptions thrown
when proving ``Proxy`` are handled by the ``<</2`` control construct.
This syntax construct supports backtracking over the ``{Proxy}`` goal.

Caveat: although the goal argument is fully compiled before calling,
some necessary information for the second compiler pass may not be
available at runtime.

Modes and number of proofs
--------------------------

::

   +object_identifier<<+callable - zero_or_more
   {+object_identifier}<<+callable - zero_or_more

Errors
------

| ``Object`` is a variable:
|     ``instantiation_error``
| ``Object`` is neither a variable nor a valid object identifier:
|     ``type_error(object_identifier, Object)``
| ``Object`` does not contain a local definition for the Goal predicate:
|     ``existence_error(procedure, Goal)``
| ``Object`` does not exist:
|     ``existence_error(object, Object)``
| ``Object`` was created/compiled with support for context switching calls turned off:
|     ``permission_error(access, database, Goal)``
| 
| ``Proxy`` is a variable:
|     ``instantiation_error``
| ``Proxy`` is neither a variable nor an object identifier:
|     ``type_error(object_identifier, Proxy)``
| The predicate ``Proxy`` does not exist in the *user* pseudo-object:
|     ``existence_error(procedure, ProxyFunctor/ProxyArity)``
|
| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

::

   % call the member/2 predicate in the
   % context of the "list" object:
   test(member) :-
       list << member(1, [1]).
