
.. index:: term_expansion/2
.. _methods_term_expansion_2:

term_expansion/2
================

Description
-----------

::

   term_expansion(Term, Expansion)

Defines an expansion for a term. This predicate, when defined and within
scope, is automatically called by the :ref:`methods_expand_term_2` method.
When that is not the case, the ``expand_term/2`` method only uses the
default expansions. Use of this predicate by the ``expand_term/2`` method
may be restricted by changing its default public scope.

The ``term_expansion/2`` predicate may return a list of terms. Returning
an empty list effectively suppresses the term.

Term expansion may be also be applied when compiling source files by
defining the object providing access to the ``term_expansion/2`` clauses
as a :term:`hook object`. Clauses for the
``term_expansion/2`` predicate defined within an object or a category
are **never** used in the compilation of the object or the category
itself. Moreover, in this context, terms wrapped using the
:ref:`control_external_call_1` compiler bypass control
construct are not expanded and any expanded term wrapped in this control
construct will not be further expanded.

Objects and categories implementing this predicate should declare that
they implement the ``expanding`` protocol if no ancestor already
declares it. This protocol implementation relation can be declared as
either protected or private to restrict the scope of this predicate.

Template and modes
------------------

::

   term_expansion(+nonvar, -nonvar)
   term_expansion(+nonvar, -list(nonvar))

Errors
------

``(none)``

Examples
--------

``term_expansion((:- license(default)), (:- license(gplv3))).``

``term_expansion(data(Millimeters), data(Meters)) :- Meters is Millimeters / 1000.``

.. seealso::

   :ref:`methods_expand_goal_2`,
   :ref:`methods_expand_term_2`,
   :ref:`methods_goal_expansion_2`,
   :ref:`predicates_logtalk_load_context_2`
