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


.. index:: pair: {}/1; Control construct
.. _control_external_call_1:

``{}/1``
========

Description
-----------

::

   {Term}
   {Goal}

This control construct allows the programmer to bypass the Logtalk compiler.
It can also be used to wrap a source file term (either a clause or a directive)
to bypass the :ref:`term-expansion mechanism <expansion_expansion>`. Similarly,
it can also be used to wrap a goal to bypass the goal-expansion mechanism. When
used to wrap a goal, it is opaque to cuts and the argument is called within
the context of the pseudo-object :ref:`user <objects_user>`. It is also possible
to use ``{Closure}`` as the first argument of :ref:`methods_call_N` calls. In
this case, ``Closure`` will be extended with the remaining arguments of
the ``call/2-N`` call in order to construct a goal that will be called
within the context of ``user``. It can also be used as a message to any
object. This is useful when the message is e.g. a conjunction of
messages, some of which being calls to Prolog built-in predicates.

This control construct may also be used in place of an object identifier
when sending a message. In this case, the result of proving its argument
as a goal (within the context of the pseudo-object ``user``) is used as
an object identifier in the message sending call. This feature is mainly
used with :term:`parametric objects <parametric object>` when their
identifiers correspond to predicates defined in ``user``.

Modes and number of proofs
--------------------------

::

   {+callable} - zero_or_more

Errors
------

| ``Term`` is a variable:
|     ``instantiation_error``
| ``Term`` is neither a variable nor a callable term:
|     ``type_error(callable, Term)``
| 
| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

::

   % bypass the compiler for the next term:
   {:- load_foreign_resource(file)}.

   % overload the standard </2 operator: 
   N1/D1 < N2/D2 :-
       {N1*D2 < N2*D1}.

   % call a closure in the context of "user":
   call_in_user(F, X, Y, Z) :-
       call({F}, X, Y, Z).

   % use parametric object proxies:
   | ?- {circle(Id, Radius, Color)}::area(Area).
   ...

   % use Prolog built-in predicates as messages:
   | ?- logtalk::{write('hello world!'), nl}.
   hello world!
   yes
