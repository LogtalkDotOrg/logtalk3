..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: expand_goal/2; Built-in method
.. _methods_expand_goal_2:

``expand_goal/2``
=================

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

Modes and number of proofs
--------------------------

::

   expand_goal(?term, ?term) - one

Errors
------

(none)

Examples
--------

(none)

.. seealso::

   :ref:`methods_expand_term_2`,
   :ref:`methods_goal_expansion_2`,
   :ref:`methods_term_expansion_2`
