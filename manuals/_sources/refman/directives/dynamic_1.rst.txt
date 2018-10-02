
.. _directives_dynamic_1:

dynamic/1
=========

Description
-----------

::

   dynamic(Name/Arity)
   dynamic((Functor1/Arity1, ...))
   dynamic([Functor1/Arity1, ...])

   dynamic(Entity::Name/Arity)
   dynamic((Entity1::Functor1/Arity1, ...))
   dynamic([Entity1::Functor1/Arity1, ...])

   dynamic(Module:Name/Arity)
   dynamic((Module1:Functor1/Arity1, ...))
   dynamic([Module1:Functor1/Arity1, ...])

   dynamic(Name//Arity)
   dynamic((Functor1//Arity1, ...))
   dynamic([Functor1//Arity1, ...])

   dynamic(Entity::Name//Arity)
   dynamic((Entity1::Functor1//Arity1, ...))
   dynamic([Entity1::Functor1//Arity1, ...])

   dynamic(Module:Name//Arity)
   dynamic((Module1:Functor1//Arity1, ...))
   dynamic([Module1:Functor1//Arity1, ...])

Declares dynamic predicates and dynamic grammar rule non-terminals. Note
that an object can be static and have both static and dynamic
predicates/non-terminals. Dynamic predicates cannot be declared as
synchronized. When the dynamic predicates are local to an object,
declaring them also as private predicates allows the Logtalk compiler to
generate optimized code for asserting and retracting predicate clauses.
Categories can also contain dynamic predicate directives but cannot
contain clauses for dynamic predicates.

The predicate indicators (or non-terminal indicators) can be explicitly
qualified with an object, category, or module identifier when the
predicates (or non-terminals) are also declared multifile.

Note that dynamic predicates cannot be declared synchronized (when
necessary, declare the predicates updating the dynamic predicates as
synchronized).

Template and modes
------------------

::

   dynamic(+qualified_predicate_indicator_term)
   dynamic(+qualified_non_terminal_indicator_term)

Examples
--------

::

   :- dynamic(counter/1).

   :- dynamic((lives/2, works/2)).

   :- dynamic([db/4, key/2, file/3]).

See also
--------

:ref:`directives_dynamic_0`
