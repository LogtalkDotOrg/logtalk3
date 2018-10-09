
.. index:: public/1
.. _directives_public_1:

public/1
========

Description
-----------

::

   public(Name/Arity)
   public((Functor1/Arity1, ...))
   public([Functor1/Arity1, ...])

   public(Name//Arity)
   public((Functor1//Arity1, ...))
   public([Functor1//Arity1, ...])

   public(op(Precedence, Associativity, Operator))

Declares public predicates, public grammar rule non-terminals, and
public operators. A public predicate can be called from any object. A
public non-terminal can be used as an argument in
:ref:`methods_phrase_2` and
:ref:`methods_phrase_3` calls from any object.
Public operators are not exported but declaring them provides useful
information for defining client objects.

Template and modes
------------------

::

   public(+predicate_indicator_term)
   public(+non_terminal_indicator_term)
   public(+operator_declaration)

Examples
--------

::

   :- public(ancestor/1).

   :- public((instance/1, instances/1)).

   :- public([leaf/1, leaves/1]).

.. seealso::

   :ref:`directives_private_1`,
   :ref:`directives_protected_1`
