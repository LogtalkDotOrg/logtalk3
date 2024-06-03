..
   This file is part of Logtalk <https://logtalk.org/>  
   SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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

**directive**

.. index:: pair: op/3; Directive
.. _directives_op_3:

``op/3``
========

Description
-----------

::

   op(Precedence, Associativity, Operator)
   op(Precedence, Associativity, [Operator, ...])

Declares operators. Global operators can be declared inside a source file
by writing the respective directives before the entity opening directives.
Operators declared inside entities have local scope. Calls to the standard
term input and output predicates take into account any locally defined
operators.

Template and modes
------------------

::

   op(+integer, +associativity, +atom_or_atom_list)

Examples
--------

Some of the predicate argument instantiation mode operators used by Logtalk:

::

   :- op(200, fy, +).
   :- op(200, fy, ?).
   :- op(200, fy, @).
   :- op(200, fy, -).

An example of using entity local operators. Consider the following ``ops.lgt``
file:

::

   :- initialization((write(<=>(1,2)), nl)).
   
   :- object(ops).
   
       :- op(700, xfx, <=>).
   
       :- public(w/1).
       w(Term) :-
           write(Term), nl.
   
       :- public(r/1).
       r(Term) :-
           read(Term).

   :- end_object.

Loading the file automatically calls the initialization goal. Compare its
output with the output of the ``ops::w/1`` predicate. Compare also reading
a term from within the ``ops`` object versus reading from ``user``.

.. code-block:: text

   | ?- {ops}.
   <=>(1,2)
   true.

   | ?- ops::w(<=>(1,2)).
   1<=>2
   true.

   | ?- ops::r(T).
   |: 3<=>4.
   
   T = <=>(3, 4).

   | ?- read(T).
   |: 5<=>6.
   
   SYNTAX ERROR: operator expected

.. seealso::

   :ref:`methods_current_op_3`
