..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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

**built-in method**

.. index:: pair: expand_term/2; Built-in method
.. _methods_expand_term_2:

``expand_term/2``
=================

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

Modes and number of proofs
--------------------------

::

   expand_term(?term, ?term) - one

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
