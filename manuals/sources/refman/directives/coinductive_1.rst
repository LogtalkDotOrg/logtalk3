
.. index:: coinductive/1
.. _directives_coinductive_1:

coinductive/1
=============

Description
-----------

::

   coinductive(Name/Arity)
   coinductive((Functor1/Arity1, ...))
   coinductive([Functor1/Arity1, ...])

   coinductive(Template)
   coinductive((Template1, ...))
   coinductive([Template1, ...])

This is an **experimental** directive, used for declaring coinductive
predicates. Requires a back-end Prolog compiler with minimal support for
cyclic terms. The current implementation of coinduction allows the
generation of only the *basic cycles* but all valid solutions should be
recognized. Use a predicate indicator as argument when all the
coinductive predicate arguments are relevant for coinductive success.
Use a template when only some coinductive predicate arguments
(represented by a "``+``") should be considered when testing for
coinductive success (represent the arguments that should be disregarded
by a "``-``"). It's possible to define local
:ref:`methods_coinductive_success_hook_1_2`
predicates that are automatically called with the coinductive predicate
term resulting from a successful unification with an ancestor goal as
first argument. The second argument, when present, is the coinductive
hypothesis (i.e. the ancestor goal) used. These hook predicates can
provide an alternative to the use of tabling when defining some
coinductive predicates. There is no overhead when these hook predicates
are not defined.

This directive must precede any calls to the declared coinductive
predicates.

Template and modes
------------------

::

   coinductive(+predicate_indicator_term)
   coinductive(+coinductive_predicate_template_term)

Examples
--------

::

   :- coinductive(comember/2).
   :- coinductive(controller(+,+,+,-,-)).

.. seealso::

   :ref:`methods_coinductive_success_hook_1_2`
