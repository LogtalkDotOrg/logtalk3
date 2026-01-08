..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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

**built-in predicate**

.. index:: pair: logtalk_load_context/2; Built-in predicate
.. _predicates_logtalk_load_context_2:

``logtalk_load_context/2``
==========================

Description
-----------

::

   logtalk_load_context(Key, Value)

Provides access to the Logtalk compilation/loading context. The following keys
are currently supported:

* ``entity_identifier`` - identifier of the entity being compiled if any
* ``entity_prefix`` - internal prefix for the entity compiled code
* ``entity_type`` - returns the value ``module`` when compiling a module as an object
* ``entity_relation`` - returns the entity relations as declared in the entity opening directive
* ``source`` - full path of the source file being compiled
* ``file`` - the actual file being compiled, different from ``source`` only when processing an ``include/1`` directive
* ``basename`` - source file basename
* ``directory`` - source file directory
* ``stream`` - input stream being used to read source file terms
* ``target`` - the full path of the intermediate Prolog file
* ``flags`` - the list of the explicit flags used for the compilation of the source file
* ``term`` - the source file term being compiled
* ``term_position`` - the position of the term being compiled (``StartLine-EndLine``)
* ``variables`` - the variables of the term being compiled (``[Variable1, ...]``)
* ``variable_names`` - the variable names of the term being compiled (``[Name1=Variable1, ...]``)
* ``variable_names(Term)`` - the variable names of the term being compiled (``[Name1=Variable1, ...]``)
* ``singletons`` - the singleton variables of the term being compiled (``[Name1=Variable1, ...]``)
* ``singletons(Term)`` - the singleton variables of the term being compiled (``[Name1=Variable1, ...]``)
* ``parameter_variables`` - list of parameter variable names and positions (``[Name1-Position1, ...]``)

For the ``entity_relation`` key, the possible values are:

* ``extends_protocol(Protocol, ParentProtocol, Scope)``
* ``implements_protocol(ObjectOrCategory, Protocol, Scope)``
* ``extends_category(Category, ParentCategory, Scope)``
* ``imports_category(Object, Category, Scope)``
* ``extends_object(Prototype, Parent, Scope)``
* ``instantiates_class(Instance, Class, Scope)``
* ``specializes_class(Class, Superclass, Scope)``
* ``complements_object(Category, Object)``

Calling this predicate with the ``parameter_variables`` key only succeeds
when compiling a parametric entity containing parameter variables.

This predicate is usually called by the :ref:`methods_term_expansion_2`
and :ref:`methods_goal_expansion_2` methods. It can also be called directly
from :ref:`directives_initialization_1` directives in a source file. Note
that the entity keys are only available when compiling an entity term or
from an object ``initialization/1`` directive.

.. warning::

   The ``term_position`` key is only supported in
   :term:`backend Prolog compilers <backend Prolog compiler>`
   that provide access to the start and end lines of a read term. When
   such support is not available, the value ``-1`` is returned for both
   the start and the end lines.

   Variables in the values of the ``term``, ``variables``, ``variable_names``,
   and ``singletons`` keys are not shared with, respectively, the term and
   goal arguments of the ``term_expansion/2`` and ``goal_expansion/2`` methods.
   Use instead the ``variable_names(Term)`` and ``singletons(Term)`` keys when
   possible.

Modes and number of proofs
--------------------------

::

   logtalk_load_context(?callable, -nonvar) - zero_or_more

Errors
------

| ``Key`` is neither a variable nor a callable term:
|     ``type_error(callable, Key)``
| ``Key`` is a callable term but not a valid key:
|     ``domain_error(logtalk_load_context_key, Key)``

Examples
--------

::

   % expand source file terms only if they are entity terms
   term_expansion(Term, ExpandedTerms) :-
       logtalk_load_context(entity_identifier, _),
       ....

   % expand source file term while accessing its variable names
   term_expansion(Term, ExpandedTerms) :-
       logtalk_load_context(variable_names(Term), VariableNames),
       ....

   % define a library alias based on the source directory
   :- initialization((
       logtalk_load_context(directory, Directory),
       assertz(logtalk_library_path(my_app, Directory))
   )).

.. seealso::

   :ref:`methods_term_expansion_2`,
   :ref:`methods_goal_expansion_2`,
   :ref:`directives_initialization_1`
