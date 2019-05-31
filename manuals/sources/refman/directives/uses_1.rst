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


.. index:: pair: uses/1; Directive
.. _directives_uses_1:

uses/1
======

Description
-----------

::

   uses([Object as Alias, ...])

Declares object aliases. Typically used to shorten long object names or to
simplify experimenting with different object implementations of the same
protocol when using explicit message sending.

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

   :- uses([
       difflist as dl,
       fast_random as rnd
   ]).

   foo :-
       ...,
       % the same as difflist::append(L1, L2, L)
       dl::append(L1, L2, L),
       % the same as fast_random::permutation(L, P)
       rnd::permutation(L, P),
       ...

.. seealso::

   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
