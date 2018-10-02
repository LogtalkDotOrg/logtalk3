
.. index:: protected/1
.. _directives_protected_1:

protected/1
===========

Description
-----------

::

   protected(Name/Arity)
   protected((Functor1/Arity1, ...))
   protected([Functor1/Arity1, ...])

   protected(Name//Arity)
   protected((Functor1//Arity1, ...))
   protected([Functor1//Arity1, ...])

   protected(op(Precedence, Associativity, Operator))

Declares protected predicates, protected grammar rule non-terminals, and
protected operators. A protected predicate can only be called from the
object containing the directive or from an object that inherits the
directive. A protected non-terminal can only be used as an argument in a
:ref:`methods_phrase_2` and
:ref:`methods_phrase_3` calls from the object
containing the directive or from an object that inherits the directive.
Protected operators are not inherited but declaring them provides useful
information for defining descendant objects.

Template and modes
------------------

::

   protected(+predicate_indicator_term)
   protected(+non_terminal_indicator_term)
   protected(+operator_declaration)

Examples
--------

::

   :- protected(init/1).

   :- protected((print/2, convert/4)).

   :- protected([load/1, save/3]).

See also
--------

:ref:`directives_private_1`,
:ref:`directives_public_1`
