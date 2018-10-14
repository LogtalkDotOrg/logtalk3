..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


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

Modes and number of proofs
--------------------------

::

   threaded(+callable) - zero_or_one

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
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_ignore_1`,
   :ref:`directives_synchronized_1`
