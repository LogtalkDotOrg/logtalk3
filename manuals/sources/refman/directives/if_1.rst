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


.. index:: pair: if/1; Directive
.. _directives_if_1:

``if/1``
========

Description
-----------

::

   if(Goal)

Starts conditional compilation. The code following the directive is
compiled iff ``Goal`` is true. The goal is subjected to
:ref:`goal expansion <expansion_expansion>` when the directive occurs
in a source file. Conditional compilation directives can be nested.

Conditional compilation goals cannot depend on predicate definitions
contained in the same source file that contains the conditional
compilation directives (as those predicates only become available after
the file is fully compiled and loaded).

Template and modes
------------------

::

   if(@callable)

Examples
--------

A common example is checking if a built-in predicate exists and
providing a definition when the predicate is absent:

::

   :- if(\+ predicate_property(length(_,_), built_in)).

       length(List, Length) :-
           ...

   :- endif.

Another common example is conditionally including code for a specific
backend Prolog compiler:

::

   :- if(current_logtalk_flag(prolog_dialect, swi)).

       % SWI-Prolog specific code
       :- set_prolog_flag(double_quotes, codes).

   :- endif.

.. seealso::

   :ref:`directives_elif_1`,
   :ref:`directives_else_0`,
   :ref:`directives_endif_0`
