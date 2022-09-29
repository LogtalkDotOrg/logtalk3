..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


.. rst-class:: align-right

**control construct**

.. index:: pair: {}/1; Control construct
.. _control_external_call_1:

``{}/1``
========

Description
-----------

::

   {Goal}
   {Closure}
   {Term}

This control construct allows the programmer to bypass the Logtalk compiler
(including its linter but not its optimizer) in multiple contexts:

- Calling a goal as-is (from within an object or category) in the context of
  the :ref:`user <objects_user>` pseudo-object.

- Extending a closure as-is with the remaining arguments of a
  :ref:`call/2-N <methods_call_N>` call in order to construct a goal that will
  be called within the context of the ``user`` pseudo-object.

- Wrapping a source file term (either a clause or a directive) or a source file
  goal to bypass the :ref:`term-expansion mechanism <expansion_expansion>`.

- Using it in place of an object identifier when sending a message. In this
  case, its argument is proved as a goal within the context of the ``user``
  pseudo-object with the resulting term being used as an object identifier
  in the message sending goal. This feature is mainly used with
  :term:`parametric objects <parametric object>` when their identifiers
  correspond to predicates defined in ``user``.

- Using it as a message to an object. This is mainly useful when the message is
  e.g. a :ref:`conjunction of messages <messages_broadcasting>`, some of which
  being calls to Prolog built-in predicates.

.. note::

   This control construct is opaque to cuts when used to wrap a goal (thus
   ensuring the same semantics independently of the argument being bound at
   compile time or at runtime).

Modes and number of proofs
--------------------------

::

   {+callable} - zero_or_more

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``
| 
| ``Closure`` is a variable:
|     ``instantiation_error``
| ``Closure`` is neither a variable nor a callable term:
|     ``type_error(callable, Closure)``
| 
| ``Term`` is a variable:
|     ``instantiation_error``
| ``Term`` is neither a variable nor a callable term:
|     ``type_error(callable, Term)``

Examples
--------

::

   % overload the standard (<)/2 operator by
   % calling its standard built-in definition:
   N1/D1 < N2/D2 :-
       {N1*D2 < N2*D1}.

   % call a closure in the context of "user":
   call_in_user(F, X, Y, Z) :-
       call({F}, X, Y, Z).

   % bypass the compiler for a proprietary backend directive:
   {:- load_foreign_resource(file)}.

   % use parametric object proxies:
   | ?- {circle(Id, Radius, Color)}::area(Area).
   ...

   % use Prolog built-in predicates as messages:
   | ?- logtalk::{write('hello world!'), nl}.
   hello world!
   yes
