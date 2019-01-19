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


.. _performance_performance:

Performance
===========

Logtalk is implemented as a *trans-compiler* to Prolog. When compiling
predicates, it preserves in the generated Prolog code all cases of
first-argument indexing and tail-recursion. In practice, this mean that
if you know how to write efficient Prolog predicates, you already know
how to write efficient Logtalk predicates.

The Logtalk compiler adds an hidden execution-context argument to all
entity predicates. In the common case where a predicate makes no calls to
the execution-context predicates and message-sending control constructs
and is neither a meta-predicate nor a coinductive predicate, the
execution-context argument is simply passed between goals. In this case,
with most backend Prolog VMs, the cost of this extra argument is null or
negligible. When the execution-context argument needs to be accessed
(e.g. to fetch the value of *self* for a :ref:`control_send_to_self_1`
call) there may be a small inherent overhead due to the implicit unifications.

Local predicate calls
---------------------

Local calls to object (or category) predicates have zero overhead in
terms of number of inferences, as expected, compared with local Prolog
calls.

Calls to imported or inherited predicates
-----------------------------------------

Assuming the :ref:`optimize <flag_optimize>` flag is turned on and a
static predicate, :ref:`control_call_super_1` calls have zero overhead
in terms of number of inferences.

Calls to module predicates
--------------------------

Local calls from an object (or category) to a module predicate have zero
overhead (assuming both the module and the predicate are bound at
compile time).

Messages
--------

For ``::/1-2`` calls Logtalk implements static binding and dynamic
binding. For dynamic binding, a caching mechanism is used by the
runtime. It's useful to measure the performance overhead in *number of
inferences* compared with plain Prolog and Prolog modules. The results
for Logtalk 3.17.0 and later versions are:

-  Static binding: 0
-  Dynamic binding (object bound at compile time): 1
-  Dynamic binding (object bound at runtime time): 2

Static binding is the common case with libraries and most application
code; it requires compiling code with the :ref:`optimize <flag_optimize>`
flag turned on. Dynamic binding numbers are after the first call (i.e.
after the generalization of the query is cached). All numbers with the
:ref:`events <flag_events>` flag set to ``deny`` (setting this flag to
``allow`` adds an overhead of 5 inferences to the results above).

The dynamic binding caches assume the used :term:`backend Prolog compiler`
does indexing of dynamic predicates. This is a common feature of modern
Prolog systems but the actual details vary from system to system and may
have an impact on dynamic binding performance.

Note that messages to *self* (:ref:`control_send_to_self_1` calls) always
use dynamic binding as the object that receives the message is only know
at runtime.

Inlining
--------

When the :ref:`optimize <flag_optimize>` flag is turned on, the Logtalk
compiler performs *inlining* of predicate calls whenever possible. This
includes calls to built-in predicates such as ``once/1``, ``ignore/1``,
and ``phrase/2-3`` but also calls to Prolog predicates that are either
built-in, foreign, or defined in a module (including ``user``). Inlining
notably allows wrapping module or foreign predicates using an object without
introducing any overhead. In the specific case of the
:ref:`execution-context predicates <predicates_context>`,
calls are inlined independently of the ``optimize`` flag value.

Generated code simplification and optimizations
-----------------------------------------------

When the :ref:`optimize <flag_optimize>` flag is turned on, the Logtalk
compiler simplifies and optimizes generated clauses (including those
resulting from the compilation of grammar rules), by flattening conjunctions,
folding left unifications (e.g. generated as a by-product of the compilation
of grammar rules), and removing redundant calls to ``true/0``.

Other considerations
--------------------

One aspect of performance, that affects both Logtalk and Prolog code, is
the characteristics of the Prolog VM. The Logtalk distribution includes
two examples,
`bench <https://github.com/LogtalkDotOrg/logtalk3/tree/master/examples/bench>`_
and
`benchmarks <https://github.com/LogtalkDotOrg/logtalk3/tree/master/examples/benchmarks>`_,
to help evaluate performance with specific backend Prolog systems. A
table with `results <https://logtalk.org/performance.html>`_ for a
subset of the supported systems is also available in the Logtalk
website.
