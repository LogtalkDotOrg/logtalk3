
.. index:: discontiguous/1
.. _directives_discontiguous_1:

discontiguous/1
===============

Description
-----------

::

   discontiguous(Name/Arity)
   discontiguous((Functor1/Arity1, ...))
   discontiguous([Functor1/Arity1, ...])

   discontiguous(Name//Arity)
   discontiguous((Functor1//Arity1, ...))
   discontiguous([Functor1//Arity1, ...])

Declares discontiguous predicates and discontiguous grammar rule
non-terminals. The use of this directive should be avoided as not all
backend Prolog compilers support discontiguous predicates.

Template and modes
------------------

::

   discontiguous(+predicate_indicator_term)
   discontiguous(+non_terminal_indicator_term)

Examples
--------

::

   :- discontiguous(counter/1).

   :- discontiguous((lives/2, works/2)).

   :- discontiguous([db/4, key/2, file/3]).
