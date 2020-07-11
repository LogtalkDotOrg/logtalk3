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


.. index:: pair: uses/1; Directive
.. _directives_uses_1:

uses/1
======

Description
-----------

::

   uses([Object as Alias, ...])

Declares object aliases. Typically used to shorten long object names, to
simplify and consistently send messages to parameterized objects, and to
simplify using or experimenting with different object implementations of
the same protocol when using explicit message sending. Object aliases are
local to the object (or category) where they are defined.

The objects being aliased can be :term:`parameter variables <parameter variable>`
or parametric objects where one of more parameters are parameter variables
when using the directive in a parametric object or a parametric category
defined in a source file (the common case).

Declaring multiple aliases for the same object is allowed. But repeated
declarations of the same alias, declaring an alias for an object alias,
and redefining an alias to reference a different object are reported as
compilation errors.

To enable the use of static binding, and thus optimal message sending
performance, the objects should be loaded before compiling the entities
that call their predicates.

Template and modes
------------------

::

   uses(+object_alias_list)

Examples
--------

::

   :- object(foo(_HeapType_, _OptionsObject_)).

       :- uses([
           fast_random as rnd,
           time(utc) as time,
           heap(_HeapType_) as heap,
           _OptionsObject_ as options
       ]).

       bar :-
           ...,
           % the same as fast_random::permutation(L, P)
           rnd::permutation(L, P),
           % the same as heap(_HeapType_)::as_heap(L, H)
           heap::as_heap(L, H),
           % the same as _OptionsObject_::get(foo, X)
           options::get(foo, X),
           % the same as time(utc)::now(T)
           time::now(T),
           ...

.. seealso::

   :ref:`directives_uses_2`,
   :ref:`directives_use_module_1`,
   :ref:`directives_use_module_2`
