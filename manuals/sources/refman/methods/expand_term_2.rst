
.. index:: expand_term/2
.. _methods_expand_term_2:

expand_term/2
=============

Description
-----------

::

   expand_term(Term, Expansion)

Expands a term. The most common use is to expand a grammar rule into a
clause. Users may override the default Logtalk grammar rule translator
by defining clauses for the :ref:`methods_term_expansion_2` hook predicate.

The expansion works as follows: if the first argument is a variable,
then it is unified with the second argument; if the first argument is
not a variable and there are local or inherited clauses for the
``term_expansion/2`` hook predicate within scope, then this predicate is
called to provide an expansion that is then unified with the second
argument; if the ``term_expansion/2`` predicate is not used and the
first argument is a compound term with functor ``-->/2`` then the
default Logtalk grammar rule translator is used, with the resulting
clause being unified with the second argument; when the translator is
not used, the two arguments are unified. The ``expand_term/2`` predicate
may return a single term or a list of terms.

This built-in method may be used to expand a grammar rule into a clause
for use with the built-in database methods.

Automatic term expansion is only performed at compile time (to expand
terms read from a source file) when using a :term:`hook object`. This
predicate can be used by the user to manually perform term expansion
at runtime (for example, to convert a grammar rule into a clause).

Template and modes
------------------

::

   expand_term(?term, ?term)

Errors
------

(none)

Examples
--------

(none)

.. seealso::

   :ref:`methods_expand_goal_2`,
   :ref:`methods_goal_expansion_2`,
   :ref:`methods_term_expansion_2`
