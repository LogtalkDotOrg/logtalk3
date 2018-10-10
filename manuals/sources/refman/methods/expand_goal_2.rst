
.. index:: expand_goal/2
.. _methods_expand_goal_2:

expand_goal/2
=============

Description
-----------

::

   expand_goal(Goal, ExpandedGoal)

Expands a goal. The expansion works as follows: if the first argument
is a variable, then it is unified with the second argument; if the first
argument is not a variable and there are local or inherited clauses for
the ``goal_expansion/2`` hook predicate within scope, then this predicate
is recursively called until a fixed-point is reached to provide an
expansion that is then unified with the second argument; if the
``goal_expansion/2`` predicate is not within scope, the two arguments
are unified.

Automatic goal expansion is only performed at compile time (to expand
the body of clauses and meta-directives read from a source file) when
using :term:`hook objects <hook object>`. This predicate can be
used by the user to manually perform goal expansion at runtime (for
example, before asserting a clause).

Template and modes
------------------

::

   expand_goal(?term, ?term)

Errors
------

``(none)``

Examples
--------

``(none)``

.. seealso::

   :ref:`methods_expand_term_2`,
   :ref:`methods_goal_expansion_2`,
   :ref:`methods_term_expansion_2`
