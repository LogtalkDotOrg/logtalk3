..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: op/3
.. _directives_op_3:

op/3
====

Description
-----------

::

   op(Precedence, Associativity, Operator)
   op(Precedence, Associativity, [Operator, ...])

Declares operators. Operators declared inside entities have local scope.
Global operators can be declared inside a source file by writing the
respective directives before the entity opening directives.

Template and modes
------------------

::

   op(+integer, +associativity, +atom_or_atom_list)

Examples
--------

::

   :- op(200, fy, +).
   :- op(200, fy, ?).
   :- op(200, fy, @).
   :- op(200, fy, -).

.. seealso::

   :ref:`methods_current_op_3`
