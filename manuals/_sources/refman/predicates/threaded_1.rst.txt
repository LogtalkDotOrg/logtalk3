
.. index:: threaded/1
.. _predicates_threaded_1:

threaded/1
==========

Description
-----------

::

   threaded(Goals)

   threaded(Conjunction)
   threaded(Disjunction)

Proves each goal in a conjunction (disjunction) of goals in its own
thread. This predicate is deterministic and opaque to cuts. The
predicate argument is **not** flattened.

When the argument is a conjunction of goals, a call to this predicate
blocks until either all goals succeed, one of the goals fail, or one of
the goals generate an exception; the failure of one of the goals or an
exception on the execution of one of the goals results in the
termination of the remaining threads. The predicate call is true *iff*
all goals are true.

When the argument is a disjunction of goals, a call to this predicate
blocks until either one of the goals succeeds, all the goals fail, or
one of the goals generate an exception; the success of one of the goals
or an exception on the execution of one of the goals results in the
termination of the remaining threads. The predicate call is true *iff*
one of the goals is true.

When the predicate argument is neither a conjunction not a disjunction
of goals, no threads are used. In this case, the predicate call is
equivalent to a ``once/1`` predicate call.

Template and modes
------------------

::

   threaded(+callable)

Errors
------

Goals is a variable:
   ``instantiation_error``
A goal in Goals is a variable:
   ``instantiation_error``
Goals is neither a variable nor a callable term:
   ``type_error(callable, Goals)``
A goal Goal in Goals is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

Prove a conjunction of goals, each one in its own thread:
   ``threaded((Goal, Goals))``
Prove a disjunction of goals, each one in its own thread:
   ``threaded((Goal; Goals))``

.. seealso::

   :ref:`predicates_threaded_call_1_2`,
   :ref:`predicates_threaded_exit_1_2`,
   :ref:`predicates_threaded_ignore_1`,
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_peek_1_2`
