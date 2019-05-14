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


.. index:: pair: meta_predicate/1; Directive
.. _directives_meta_predicate_1:

meta_predicate/1
================

Description
-----------

::

   meta_predicate(Template)
   meta_predicate((Template, ...))
   meta_predicate([Template, ...])

   meta_predicate(Entity::Template)
   meta_predicate((Entity::Template, ...))
   meta_predicate([Entity::Template, ...])

   meta_predicate(Module:Template)
   meta_predicate((Module:Template, ...))
   meta_predicate([Module:Template, ...])

Declares meta-predicates, i.e., predicates that have arguments that will
be called as goals. An argument may also be a *closure* instead of a
goal if the meta-predicate uses the :ref:`methods_call_N` Logtalk built-in
methods to construct and call the actual goal from the closure and the
additional arguments.

Meta-arguments which are goals are represented by the integer ``0``.
Meta-arguments which are closures are represented by a positive integer,
``N``, representing the number of additional arguments that will be
appended to the closure in order to construct the corresponding
meta-call. Normal arguments are represented by the atom ``*``.
Meta-arguments are always called in the meta-predicate calling context,
not in the meta-predicate definition context.

Logtalk allows the use of this directive to override the original
meta-predicate directive. This is sometimes necessary when calling
Prolog module meta-predicates due to the lack of standardization of the
syntax of the meta-predicate templates.

.. warning::

   Some backend Prolog compilers declare ``meta_predicate`` as an operator
   for a lighter syntax. But this makes the code non-portable and is
   a practice best avoided.

Template and modes
------------------

::

   meta_predicate(+meta_predicate_template_term)

   meta_predicate(+object_identifier::+meta_predicate_template_term)
   meta_predicate(+category_identifier::+meta_predicate_template_term)

   meta_predicate(+module_identifier:+meta_predicate_template_term)

Examples
--------

::

   % findall/3 second argument is interpreted as a goal:
   :- meta_predicate(findall(*, 0, *)).

   % both forall/2 arguments are interpreted as goals:
   :- meta_predicate(forall(0, 0)).

   % maplist/3 first argument is interpreted as a closure
   % that will be expanded to a goal by appending two
   % arguments:
   :- meta_predicate(maplist(2, *, *)).

.. seealso::

   :ref:`directives_meta_non_terminal_1`,
   :ref:`methods_predicate_property_2`
