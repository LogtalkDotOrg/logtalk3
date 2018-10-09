
.. index:: alias/2
.. _directives_alias_2:

alias/2
=======

Description
-----------

::

   alias(Entity, PredicateAliases)
   alias(Entity, NonTerminalAliases)

Declares predicate and grammar rule non-terminal aliases. A predicate
(non-terminal) alias is an alternative name for a predicate
(non-terminal) declared or defined in an extended protocol, an
implemented protocol, an extended category, an imported category, an
extended prototype, an instantiated class, or a specialized class.
Predicate aliases may be used to solve conflicts between imported or
inherited predicates. It may also be used to give a predicate
(non-terminal) a name more appropriated in its usage context. This
directive may be used in objects, protocols, and categories.

Predicate (and non-terminal) aliases are specified using (preferably)
the notation ``Name/Arity as Alias/Arity`` or, in alternative, the
notation ``Name/Arity::Alias/Arity``.

It is also possible to declare predicate and grammar rule non-terminal
aliases in implicit qualification directives for sending messages
to objects and calling module predicates.

Template and modes
------------------

::

   alias(@entity_identifier, +list(predicate_indicator_alias))
   alias(@entity_identifier, +list(non_terminal_indicator_alias))

Examples
--------

::

   :- alias(list, [member/2 as list_member/2]).
   :- alias(set,  [member/2 as set_member/2]).

   :- alias(words, [singular//0 as peculiar//0]).

.. seealso::

   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
