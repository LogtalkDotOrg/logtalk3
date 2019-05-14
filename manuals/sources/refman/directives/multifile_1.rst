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


.. index:: pair: multifile/1; Directive
.. _directives_multifile_1:

multifile/1
===========

Description
-----------

::

   multifile(Name/Arity)
   multifile((Name/Arity, ...))
   multifile([Name/Arity, ...])

   multifile(Entity::Name/Arity)
   multifile((Entity::Name/Arity, ...))
   multifile([Entity::Name/Arity, ...])

   multifile(Module:Name/Arity)
   multifile((Module:Name/Arity, ...))
   multifile([Module:Name/Arity, ...])

   multifile(Name//Arity)
   multifile((Name//Arity, ...))
   multifile([Name//Arity, ...])

   multifile(Entity::Name//Arity)
   multifile((Entity::Name//Arity, ...))
   multifile([Entity::Name//Arity, ...])

   multifile(Module:Name//Arity)
   multifile((Module:Name//Arity, ...))
   multifile([Module:Name//Arity, ...])

Declares multifile predicates and multifile grammar rule non-terminals.
In the case of object or category multifile predicates, the predicate
(or non-terminal) must also have a scope directive in the object or
category holding its *primary declaration* (i.e. the declaration without
the ``Entity::`` prefix). Entities holding multifile predicate primary
declarations must be compiled and loaded prior to any entities
contributing with clauses for the multifile predicates.

Protocols cannot declare multifile predicates as protocols cannot
contain predicate definitions.

.. warning::

   Some backend Prolog compilers declare ``multifile`` as an operator
   for a lighter syntax. But this makes the code non-portable and is
   a practice best avoided.

Template and modes
------------------

::

   multifile(+qualified_predicate_indicator_term)
   multifile(+qualified_non_terminal_indicator_term)

Examples
--------

::

   :- multifile(table/3).
   :- multifile(user::hook/2).

.. seealso::

   :ref:`directives_public_1`,
   :ref:`directives_protected_1`,
   :ref:`directives_private_1`,
   :ref:`methods_predicate_property_2`
