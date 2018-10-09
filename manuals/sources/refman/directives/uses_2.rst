
.. index:: uses/2
.. _directives_uses_2:

uses/2
======

Description
-----------

::

   uses(Object, Predicates)
   uses(Object, PredicatesAndAliases)

   uses(Object, NonTerminals)
   uses(Object, NonTerminalsAndAliases)

   uses(Object, Operators)

Declares that all calls made from predicates (or non-terminals) defined
in the category or object containing the directive to the specified
predicates (or non-terminals) are to be interpreted as messages to the
specified object. Thus, this directive may be used to simplify writing
of predicate definitions by allowing the programmer to omit the
``Object::`` prefix when using the predicates listed in the directive
(as long as the calls do not occur as arguments for non-standard Prolog
meta-predicates not declared on the adapter files). It is also possible
to include operator declarations,
``op(Precedence, Associativity, Operator)``, in the second argument.

This directive is also used when compiling calls to the database and
reflection built-in methods by looking into these methods predicate
arguments.

It is possible to specify a predicate alias using the notation
``Name/Arity as Alias/Arity`` or, in alternative, the notation
``Name/Arity::Alias/Arity``. Aliases may be used either for avoiding
conflicts between predicates specified in ``use_module/2`` and
``uses/2`` directives or for giving more meaningful names considering
the using context of the predicates.

To enable the use of static binding, and thus optimal message sending
performance, the objects should be loaded before compiling the entities
that call their predicates.

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

   :- uses(list, [append/3, member/2]).
   :- uses(store, [data/2]).

   foo :-
       ...,
       findall(X, member(X, L), A),    % the same as findall(X, list::member(X, L), A)
       append(A, B, C),                % the same as list::append(A, B, C)
       assertz(data(X, C)),            % the same as store::assertz(data(X, C))
       ...

Another example, using the extended notation that allows us to define
predicate aliases:

::

   :- uses(btrees, [new/1 as new_btree/1]).
   :- uses(queues, [new/1 as new_queue/1]).

   btree_to_queue :-
       ...,
       new_btree(Tree),     % the same as btrees::new(Tree)
       new_queue(Queue),    % the same as queues::new(Queue)
       ...

.. seealso::

   :ref:`directives_use_module_2`
