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


.. index:: pair: uses/2; Directive
.. _directives_uses_2:

uses/2
======

Description
-----------

::

   uses(Object, [Name/Arity, ...])
   uses(Object, [Name/Arity as Alias/Arity, ...])

   uses(Object, [Name//Arity, ...])
   uses(Object, [Name//Arity as Alias//Arity, ...])

   uses(Object, [op(Precedence, Associativity, Operator), ...])

Declares that all calls made from predicates (or non-terminals) defined
in the category or object containing the directive to the specified
predicates (or non-terminals) are to be interpreted as messages to the
specified object. Thus, this directive may be used to simplify writing
of predicate definitions by allowing the programmer to omit the
``Object::`` prefix when using the predicates listed in the directive
(as long as the calls do not occur as arguments for non-standard Prolog
meta-predicates not declared on the adapter files). It is also possible
to include operator declarations in the second argument.

This directive is also taken into account when compiling calls to the
:ref:`database <predicates_database>` and
:ref:`reflection <predicates_reflection>` built-in methods by looking
into these methods predicate arguments if bound at compile time.

It is possible to specify a predicate alias using the notation
``Name/Arity as Alias/Arity`` or, in alternative, the notation
``Name/Arity::Alias/Arity``. Aliases may be used either for avoiding
conflicts between predicates specified in ``use_module/2`` and
``uses/2`` directives or for giving more meaningful names considering
the calling context of the predicates.

To enable the use of static binding, and thus optimal message sending
performance, the objects should be loaded before compiling the entities
that call their predicates.

The object identifier argument can also be a :term:`parameter variable`
when using the directive in a parametric object or a parametric category
defined in a source file (the common case). In this case, dynamic binding
will be used for all listed predicates (and non-terminals). The parameter
variable must be instantiated at runtime when the messages are sent.

Template and modes
------------------

::

   uses(+object_identifier, +predicate_indicator_list)
   uses(+object_identifier, +predicate_indicator_alias_list)

   uses(+object_identifier, +non_terminal_indicator_list)
   uses(+object_identifier, +non_terminal_indicator_alias_list)

   uses(+object_identifier, +operator_list)

Examples
--------

::

   :- uses(list,  [append/3, member/2]).
   :- uses(store, [data/2]).
   :- uses(user,  [table/4]).

   foo :-
       ...,
       % the same as findall(X, list::member(X, L), A)
       findall(X, member(X, L), A),
       % the same as list::append(A, B, C)
       append(A, B, C),
       % the same as store::assertz(data(X, C))
       assertz(data(X, C)),
       % call the table/4 predicate in "user"
       table(X, Y, Z, T),
       ...

Another example, using the extended notation that allows us to define
predicate aliases:

::

   :- uses(btrees, [new/1 as new_btree/1]).
   :- uses(queues, [new/1 as new_queue/1]).

   btree_to_queue :-
       ...,
       % the same as btrees::new(Tree)
       new_btree(Tree),
       % the same as queues::new(Queue)
       new_queue(Queue),
       ...

An example of using a :term:`parameter variable` in place of the object
identifier to allow using the same test set for checking multiple
implementations of the same protocol:

::

   :- object(tests(_HeapObject_),
       extends(lgtunit)).

       :- uses(_HeapObject_, [
           as_heap/2, as_list/2, valid/1, new/1,
           insert/4, insert_all/3, delete/4, merge/3,
           empty/1, size/2, top/3, top_next/5
       ])

.. seealso::

   :ref:`directives_use_module_2`,
   :ref:`directives_uses_1`,
   :ref:`directives_alias_2`
