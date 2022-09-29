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

**directive**

.. index:: pair: elif/1; Directive
.. _directives_elif_1:

``elif/1``
==========

Description
-----------

::

   elif(Goal)

Supports embedded conditionals when performing conditional compilation.
The code following the directive is compiled iff ``Goal`` is true. If
``Goal`` throws an error instead of either succeeding or failing, the
error is reported by the compiler and compilation of the enclosing source
file or entity is aborted. The goal is subjected to
:ref:`goal expansion <expansion_expansion>` when the directive occurs
in a source file.

.. warning::

   Conditional compilation goals **cannot** depend on predicate definitions
   contained in the same source file that contains the conditional
   compilation directives (as those predicates only become available after
   the file is successfully compiled and loaded).

Template and modes
------------------

::

   elif(@callable)

Examples
--------

::

   :- if(current_prolog_flag(double_quotes, codes)).

       ...

   :- elif(current_prolog_flag(double_quotes, chars)).

       ...

   :- elif(current_prolog_flag(double_quotes, atom)).

       ...

   :- endif.

.. seealso::

   :ref:`directives_else_0`,
   :ref:`directives_endif_0`,
   :ref:`directives_if_1`
