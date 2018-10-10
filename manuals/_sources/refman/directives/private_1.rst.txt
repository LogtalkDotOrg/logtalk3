
.. index:: private/1
.. _directives_private_1:

private/1
=========

Description
-----------

::

   private(Name/Arity)
   private((Functor1/Arity1, ...))
   private([Functor1/Arity1, ...])

   private(Name//Arity)
   private((Functor1//Arity1, ...))
   private([Functor1//Arity1, ...])

   private(op(Precedence, Associativity, Operator))

Declares private predicates, private grammar rule non-terminals, and
private operators. A private predicate can only be called from the
object containing the private directive. A private non-terminal can
only be used in a call of the :ref:`methods_phrase_2` and
:ref:`methods_phrase_3` methods from the object
containing the private directive.

Template and modes
------------------

::

   private(+predicate_indicator_term)
   private(+non_terminal_indicator_term)
   private(+operator_declaration)

Examples
--------

::

   :- private(counter/1).

   :- private((init/1, free/1)).

   :- private([data/3, key/1, keys/1]).

.. seealso::

   :ref:`directives_protected_1`,
   :ref:`directives_public_1`,
   :ref:`methods_predicate_property_2`
