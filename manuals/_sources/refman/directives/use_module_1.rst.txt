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


.. index:: pair: use_module/1; Directive
.. _directives_use_module_1:

use_module/1
============

Description
-----------

::

   use_module([Module as Alias, ...])

Declares module aliases. Typically used to shorten long module names and to
simplify using or experimenting with different module implementations of the
same predicates when using explicitly-qualified calls. Module aliases are local
to the object (or category) where they are defined.

The modules being aliased can be :term:`parameter variables <parameter variable>`
when using the directive in a parametric object or a parametric category
defined in a source file (the common case).

Declaring multiple aliases for the same module is allowed. But repeated
declarations of the same alias, declaring an alias for a module alias,
and redefining an alias to reference a different module are reported as
compilation errors.

To enable the use of :term:`static binding`, and thus optimal predicate call
performance, the modules should be loaded before compiling the entities
that call their predicates.

Template and modes
------------------

::

   use_module(+module_alias_list)

Examples
--------

::

   :- object(foo(_DataModule_)).

       :- use_module([
           _DataModule_ as data
       ]).

       bar :-
           ...,
           % the same as _DataModule_:xy(X, Y)
           data:xy(X, Y),
           ...

.. seealso::

   :ref:`directives_uses_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
