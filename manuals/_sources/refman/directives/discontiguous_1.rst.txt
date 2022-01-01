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


.. index:: pair: discontiguous/1; Directive
.. _directives_discontiguous_1:

``discontiguous/1``
===================

Description
-----------

::

   discontiguous(Name/Arity)
   discontiguous((Name/Arity, ...))
   discontiguous([Name/Arity, ...])

   discontiguous(Name//Arity)
   discontiguous((Name//Arity, ...))
   discontiguous([Name//Arity, ...])

Declares discontiguous predicates and discontiguous grammar rule
non-terminals. The use of this directive should be avoided as not all
:term:`backend Prolog compilers <backend Prolog compiler>` support
discontiguous predicates.

.. warning::

   Some backend Prolog compilers declare the atom ``discontiguous`` as
   an operator for a lighter syntax. But this makes the code non-portable
   and is therefore a practice best avoided.

Template and modes
------------------

::

   discontiguous(+predicate_indicator_term)
   discontiguous(+non_terminal_indicator_term)

Examples
--------

::

   :- discontiguous(counter/1).

   :- discontiguous((lives/2, works/2)).

   :- discontiguous([db/4, key/2, file/3]).
