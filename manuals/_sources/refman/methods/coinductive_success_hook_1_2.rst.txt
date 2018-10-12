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


.. index:: coinductive_success_hook/1-2
.. _methods_coinductive_success_hook_1_2:

coinductive_success_hook/1-2
============================

Description
-----------

::

   coinductive_success_hook(Head, Hypothesis)
   coinductive_success_hook(Head)

User-defined hook predicates that are automatically called in case of
coinductive success when proving a query for a coinductive predicates.
The hook predicates are called with the head of the coinductive
predicate on coinductive success and, optionally, with the hypothesis
used that to reach coinductive success.

When both hook predicates are defined, the
``coinductive_success_hook/1`` clauses are only used if no
``coinductive_success_hook/2`` clause applies. The compiler ensures zero
performance penalties when defining coinductive predicates without a
corresponding definition for the coinductive success hook predicates.

The compiler assumes that these hook predicates are defined as static
predicates in order to optimize their use.

Template and modes
------------------

::

   coinductive_success_hook(+callable, +callable)
   coinductive_success_hook(+callable)

Errors
------

(none)

Examples
--------

(none)

.. seealso::

   :ref:`directives_coinductive_1`
