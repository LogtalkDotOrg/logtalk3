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

**directive**

.. index:: pair: use_module/2; Directive
.. _directives_use_module_2:

``use_module/2``
================

Description
-----------

::

   use_module(Module, [Name/Arity, ...])
   use_module(Module, [Name/Arity as Alias/Arity, ...])

   use_module(Module, [Predicate as Alias, ...])

   use_module(Module, [Name//Arity, ...])
   use_module(Module, [Name//Arity as Alias//Arity, ...])

   use_module(Module, [op(Precedence,Associativity,Operator), ...])

This directive declares that all calls (made from predicates defined in
the category or object containing the directive) to the specified
predicates (or non-terminals) are to be interpreted as calls to
explicitly-qualified module predicates (or non-terminals). Thus, this
directive may be used to simplify writing of predicate definitions by
allowing the programmer to omit the ``Module:`` prefix when using the
predicates listed in the directive (as long as the predicate calls do
not occur as arguments for non-standard Prolog meta-predicates not
declared on the adapter files). It is also possible to include operator
declarations in the second argument.

This directive is also taken into account when compiling calls to the
:ref:`database <predicates_database>` and
:ref:`reflection <predicates_reflection>` built-in methods by looking
into these methods predicate arguments if bound at compile time.

It is possible to specify a predicate alias using the notation
``Name/Arity as Alias/Arity`` or, in alternative, the notation
``Name/Arity:Alias/Arity``. Aliases may be used either for avoiding
conflicts between predicates specified in ``use_module/2`` and
:ref:`directives_uses_2` directives or for giving more meaningful
names considering the calling context of the predicates. For predicates,
is also possible to define alias shorthands using the notation
``Predicate as Alias`` or, in alternative, the notation
``Predicate::Alias``, where ``Predicate`` and ``Alias`` are callable
terms where some or all arguments may be instantiated.

Note that this directive differs from the directive with the same name
found on some Prolog implementations by requiring the first argument to
be a module name (an atom) instead of a file specification. In Logtalk,
there's no mixing between *loading* a resource and (declaring the)
*using* (of) a resource. As a consequence, this directive doesn't
automatically load the module. Loading the module file is dependent of
the used :term:`backend Prolog compiler` and must be done separately (usually,
using a source file directive such as ``use_module/1`` or ``use_module/2``
in the entity file or preferably in the application :term:`loader file`
file). Also, note that the name of the module may differ from the name of
the module file.

.. warning::

   The modules **must** be loaded prior to the compilation of entities
   that call the module predicates. This is required in general to allow
   the compiler to check if the called module predicate is a meta-predicate
   and retrieve its meta-predicate template to ensure proper call compilation.

The module identifier argument can also be a :term:`parameter variable`
when using the directive in a parametric object or a parametric category
defined in a source file (the common case). In this case, :term:`dynamic binding`
will be used for all listed predicates (and non-terminals). The parameter
variable must be instantiated at runtime when the calls are made.

Template and modes
------------------

::

   use_module(+module_identifier, +predicate_indicator_list)
   use_module(+module_identifier, +module_predicate_indicator_alias_list)

   use_module(+module_identifier, +predicate_template_alias_list)

   use_module(+module_identifier, +non_terminal_indicator_list)
   use_module(+module_identifier, +module_non_terminal_indicator_alias_list)

   use_module(+module_identifier, +operator_list)

Examples
--------

::

   :- use_module(lists, [append/3, member/2]).
   :- use_module(store, [data/2]).
   :- use_module(user,  [foo/1 as bar/1]).

   foo :-
       ...,
       % same as findall(X, lists:member(X, L), A)
       findall(X, member(X, L), A),
       % same as lists:append(A, B, C)
       append(A, B, C),
       % same as assertz(store:data(X, C))
       assertz(data(X, C)),
       % same as retractall(user:foo(_))
       retractall(bar(_)),
       ...

Another example, using the extended notation that allows us to define
predicate aliases:

::

   :- use_module(ugraphs, [transpose_ugraph/2 as transpose/2]).

   convert_graph :-
       ...,
       % the same as ugraphs:transpose_ugraph(Graph0, Graph)
       transpose(Graph0, Graph),
       ...

An example of defining a predicate alias that is also a shorthand:

::

   :- use_module(pairs, [
       map_list_to_pairs(length, Lists, Pairs) as length_pairs(Lists, Pairs)
   ]).


An example of using a :term:`parameter variable` in place of the module
identifier to delay to runtime the actual module to use:

::

   :- object(bar(_OptionsModule_)).

       :- use_module(_OptionsModule_, [
           set/2, get/2, reset/0
       ]).

.. seealso::

   :ref:`directives_use_module_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_uses_1`,
   :ref:`directives_alias_2`
