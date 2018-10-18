..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: dynamic/1
.. _directives_dynamic_1:

dynamic/1
=========

Description
-----------

::

   dynamic(Name/Arity)
   dynamic((Name/Arity, ...))
   dynamic([Name/Arity, ...])

   dynamic(Entity::Name/Arity)
   dynamic((Entity1::Name/Arity, ...))
   dynamic([Entity1::Name/Arity, ...])

   dynamic(Module:Name/Arity)
   dynamic((Module1:Name/Arity, ...))
   dynamic([Module1:Name/Arity, ...])

   dynamic(Name//Arity)
   dynamic((Name//Arity, ...))
   dynamic([Name//Arity, ...])

   dynamic(Entity::Name//Arity)
   dynamic((Entity1::Name//Arity, ...))
   dynamic([Entity1::Name//Arity, ...])

   dynamic(Module:Name//Arity)
   dynamic((Module1:Name//Arity, ...))
   dynamic([Module1:Name//Arity, ...])

Declares dynamic predicates and dynamic grammar rule non-terminals. Note
that an object can be static and have both static and dynamic
predicates/non-terminals. Dynamic predicates cannot be declared as
synchronized. When the dynamic predicates are local to an object,
declaring them also as private predicates allows the Logtalk compiler to
generate optimized code for asserting and retracting predicate clauses.
Categories can also contain dynamic predicate directives but cannot
contain clauses for dynamic predicates.

The predicate indicators (or non-terminal indicators) can be explicitly
qualified with an object, category, or module identifier when the
predicates (or non-terminals) are also declared multifile.

Note that dynamic predicates cannot be declared synchronized (when
necessary, declare the predicates updating the dynamic predicates as
synchronized).

Template and modes
------------------

::

   dynamic(+qualified_predicate_indicator_term)
   dynamic(+qualified_non_terminal_indicator_term)

Examples
--------

::

   :- dynamic(counter/1).

   :- dynamic((lives/2, works/2)).

   :- dynamic([db/4, key/2, file/3]).

.. seealso::

   :ref:`directives_dynamic_0`,
   :ref:`methods_predicate_property_2`
