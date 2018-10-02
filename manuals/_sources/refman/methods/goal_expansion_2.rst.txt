
.. index:: goal_expansion/2
.. _methods_goal_expansion_2:

goal_expansion/2
================

Description
-----------

::

   goal_expansion(Goal, ExpandedGoal)

Defines an expansion for a goal. The first argument is the goal to be
expanded. The expanded goal is returned in the second argument. This
predicate is called recursively on the expanded goal until a fixed point
is reached. Thus, care must be taken to avoid compilation loops. This
predicate, when defined and within scope, is automatically called by the
:ref:`methods_expand_goal_2` method. Use of this predicate
by the ``expand_goal/2`` method may be restricted by changing its
default public scope.

Goal expansion may be also be applied when compiling source files by
defining the object providing access to the ``goal_expansion/2`` clauses
as a :term:`hook object`. Clauses for the
``goal_expansion/2`` predicate defined within an object or a category
are **never** used in the compilation of the object or the category
itself. Moreover, in this context, goals wrapped using the
:ref:`control_external_call_1` compiler bypass control
construct are not expanded and any expanded goal wrapped in this control
construct will not be further expanded.

Objects and categories implementing this predicate should declare that
they implement the ``expanding`` protocol if no ancestor already
declares it. This protocol implementation relation can be declared as
either protected or private to restrict the scope of this predicate.

Template and modes
------------------

::

   goal_expansion(+callable, -callable)

Errors
------

``(none)``

Examples
--------

``goal_expansion(write(Term), (write_term(Term, []), nl)).``

``goal_expansion(read(Term), (write('Input: '), {read(Term)})).``

See also
--------

:ref:`methods_expand_goal_2`,
:ref:`methods_expand_term_2`,
:ref:`methods_term_expansion_2`,
:ref:`predicates_logtalk_load_context_2`
