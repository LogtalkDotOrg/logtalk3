
.. index:: multifile/1
.. _directives_multifile_1:

multifile/1
===========

Description
-----------

::

   multifile(Name/Arity)
   multifile((Functor1/Arity1, ...))
   multifile([Functor1/Arity1, ...])

   multifile(Entity::Name/Arity)
   multifile((Entity1::Functor1/Arity1, ...))
   multifile([Entity1::Functor1/Arity1, ...])

   multifile(Module:Name/Arity)
   multifile((Module1:Functor1/Arity1, ...))
   multifile([Module1:Functor1/Arity1, ...])

   multifile(Name//Arity)
   multifile((Functor1//Arity1, ...))
   multifile([Functor1//Arity1, ...])

   multifile(Entity::Name//Arity)
   multifile((Entity1::Functor1//Arity1, ...))
   multifile([Entity1::Functor1//Arity1, ...])

   multifile(Module:Name//Arity)
   multifile((Module1:Functor1//Arity1, ...))
   multifile([Module1:Functor1//Arity1, ...])

Declares multifile predicates and multifile grammar rule non-terminals.
In the case of object or category multifile predicates, the predicate
(or non-terminal) must also have a scope directive in the object or
category holding its *primary declaration* (i.e. the declaration without
the ``Entity::`` prefix). Entities holding multifile predicate primary
declarations must be compiled and loaded prior to any entities
contributing with clauses for the multifile predicates.

Protocols cannot declare multifile predicates as protocols cannot
contain predicate definitions.

Template and modes
------------------

::

   multifile(+qualified_predicate_indicator_term)
   multifile(+qualified_non_terminal_indicator_term)

Examples
--------

::

   :- multifile(table/3).
   :- multifile(user::hook/2).

.. seealso::

   :ref:`directives_public_1`,
   :ref:`directives_protected_1`,
   :ref:`directives_private_1`,
   :ref:`methods_predicate_property_2`
