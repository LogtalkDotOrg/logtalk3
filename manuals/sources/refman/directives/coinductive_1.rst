..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


.. index:: pair: coinductive/1; Directive
.. _directives_coinductive_1:

``coinductive/1``
=================

Description
-----------

::

   coinductive(Name/Arity)
   coinductive((Name/Arity, ...))
   coinductive([Name/Arity, ...])

   coinductive(Name//Arity)
   coinductive((Name//Arity, ...))
   coinductive([Name//Arity, ...])

   coinductive(Template)
   coinductive((Template1, ...))
   coinductive([Template1, ...])

This is an **experimental** directive, used for declaring coinductive
predicates. Requires a :term:`backend Prolog compiler` with minimal support
for cyclic terms. The current implementation of coinduction allows the
generation of only the *basic cycles* but all valid solutions should be
recognized. Use a predicate indicator or a non-terminal indicator as
argument when all the coinductive predicate arguments are relevant for
coinductive success. Use a template when only some coinductive predicate
arguments (represented by a "``+``") should be considered when testing for
coinductive success (represent the arguments that should be disregarded
by a "``-``"). It's possible to define local
:ref:`methods_coinductive_success_hook_1_2`
predicates that are automatically called with the coinductive predicate
term resulting from a successful unification with an ancestor goal as
first argument. The second argument, when present, is the coinductive
hypothesis (i.e. the ancestor goal) used. These hook predicates can
provide an alternative to the use of tabling when defining some
coinductive predicates. There is no overhead when these hook predicates
are not defined.

This directive must precede any calls to the declared coinductive
predicates.

Template and modes
------------------

::

   coinductive(+predicate_indicator_term)
   coinductive(+non_terminal_indicator_term)
   coinductive(+coinductive_predicate_template_term)

Examples
--------

::

   :- coinductive(comember/2).
   :- coinductive(ones_and_zeros//0).
   :- coinductive(controller(+,+,+,-,-)).

.. seealso::

   :ref:`methods_coinductive_success_hook_1_2`,
   :ref:`methods_predicate_property_2`
