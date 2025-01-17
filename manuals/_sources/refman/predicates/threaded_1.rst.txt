..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. rst-class:: align-right

**built-in predicate**

.. index:: pair: threaded/1; Built-in predicate
.. _predicates_threaded_1:

``threaded/1``
==============

Description
-----------

::

   threaded(Conjunction)
   threaded(Disjunction)

Proves each goal in a conjunction or a disjunction of goals in its own
thread. This meta-predicate is deterministic and opaque to cuts. The
predicate argument is **not** flattened.

When the argument is a conjunction of goals, a call to this predicate
blocks until either all goals succeed, one of the goals fail, or one of
the goals generate an exception; the failure of one of the goals or an
exception on the execution of one of the goals results in the
termination of the remaining threads. The predicate call is true *iff*
all goals are true. The predicate call fails if all goals fail. When
one of the goals throws an exception, the predicate call re-throws that
exception.

When the argument is a disjunction of goals, a call to this predicate
blocks until either one of the goals succeeds or all the goals fail or
throw exceptions; the success of one of the goals results in the
termination of the remaining threads. The predicate call is true *iff*
one of the goals is true. The predicate call fails if all goals fails.
When no goal succeeds and one of the goals throws an exception, the
predicate call re-throws that exception.

When the predicate argument is neither a conjunction nor a disjunction
of goals, no threads are used. In this case, the predicate call is
equivalent to a ``once/1`` predicate call.

A dedicated message queue is used per call of this predicate to collect
the individual goal results.

.. note::

   This predicate requires a :term:`backend Prolog compiler` providing
   compatible multi-threading primitives. The value of the read-only
   :ref:`threads <flag_threads>` flag is set to ``supported`` when that
   is the case.

Meta-predicate template
-----------------------

::

   threaded(0)

Modes and number of proofs
--------------------------

::

   threaded(+callable) - zero_or_one

Errors
------

| ``Goals`` is a variable:
|     ``instantiation_error``
| A goal in ``Goals`` is a variable:
|     ``instantiation_error``
| ``Goals`` is neither a variable nor a callable term:
|     ``type_error(callable, Goals)``
| A goal ``Goal`` in ``Goals`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

| Prove a conjunction of goals, each one in its own thread:
|     ``threaded((Goal, Goals))``
| Prove a disjunction of goals, each one in its own thread:
|     ``threaded((Goal; Goals))``

.. seealso::

   :ref:`predicates_threaded_call_1_2`,
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_ignore_1`,
   :ref:`directives_synchronized_1`
