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

The Logtalk compiler appends a hidden execution-context argument to all
entity predicate clauses. In the common case where a predicate makes no
calls to the execution-context predicates and message-sending control
constructs and is neither a meta-predicate nor a coinductive predicate,
the execution-context argument is simply passed between goals. In this
case, with most backend Prolog virtual machines, the cost of this extra
argument is null or negligible. When the execution-context needs to be
accessed (e.g. to fetch the value of *self* for a :ref:`control_send_to_self_1`
call) there may be a small inherent overhead due to the access to the
individual arguments of the compound term used to represent the
execution-context.

Source code compilation modes
-----------------------------

Source code can be compiled in *optimal*, *normal*, or *debug* mode,
depending on the :ref:`optimize <flag_optimize>` and
:ref:`debug <flag_debug>` compiler flags. Optimal mode is used when
deploying an application while normal and debug modes are used when
developing an application. Compiling code in optimal mode enables
several optimizations, notably use of :term:`static binding` whenever
enough information is available at compile time. In debug mode, most
optimizations are turned off and the code is instrumented to generate
*debug events* that enable tools such as the command-line debugger and
the ports profiler.

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

Logtalk implements static binding and dynamic binding for message sending
calls. For dynamic binding, a caching mechanism is used by the runtime.
It's useful to measure the performance overhead in *number of inferences*
compared with plain Prolog and Prolog modules. The results for Logtalk
3.17.0 and later versions are:

-  Static binding: 0
-  Dynamic binding (object bound at compile time): 1
-  Dynamic binding (object bound at runtime time): 2

Static binding is the common case with libraries and most application
code; it requires compiling code with the :ref:`optimize <flag_optimize>`
flag turned on. Dynamic binding numbers are after the first call (i.e.
after the generalization of the query is cached). All numbers with the
:ref:`events <flag_events>` flag set to ``deny`` (setting this flag to
``allow`` adds an overhead of 5 inferences to the results above; note
that this flag can be defined in a per-object basis as needed instead
of globally and thus minimizing the performance impact).

The dynamic binding caches assume the used :term:`backend Prolog compiler`
does indexing of dynamic predicates. This is a common feature of modern
Prolog systems but the actual details vary from system to system and may
have an impact on dynamic binding performance.

Note that messages to *self* (:ref:`control_send_to_self_1` calls) always
use dynamic binding as the object that receives the message is only know
at runtime.

Messages sent from Prolog modules may use static binding depending on the
used backend Prolog compiler when the ``optimize`` flag is turned on.
Consult the Prolog compiler adapter file notes for details.

Automatic expansion of built-in meta-predicates
-----------------------------------------------

The compiler always expands calls to the :ref:`methods_forall_2`,
:ref:`methods_once_1`, and :ref:`methods_ignore_1` meta-predicates into
equivalent definitions using the negation and conditional control constructs.
It also expands calls to the :ref:`methods_call_N`, :ref:`methods_phrase_2`,
and :ref:`methods_phrase_3` meta-predicates when the first argument is bound.

Inlining
--------

When the :ref:`optimize <flag_optimize>` flag is turned on, the Logtalk
compiler performs *inlining* of predicate calls whenever possible. This
includes calls to Prolog predicates that are either built-in, foreign, or
defined in a module (including ``user``). Inlining notably allows wrapping
module or foreign predicates using an object without introducing any
overhead. In the specific case of the 
:ref:`execution-context predicates <predicates_context>`,
calls are inlined independently of the ``optimize`` flag value.

Generated code simplification and optimizations
-----------------------------------------------

When the :ref:`optimize <flag_optimize>` flag is turned on, the Logtalk
compiler simplifies and optimizes generated clauses (including those
resulting from the compilation of grammar rules), by flattening conjunctions,
folding left unifications (e.g. generated as a by-product of the compilation
of grammar rules), and removing redundant calls to ``true/0``.

Size of the generated code
--------------------------

The size of the intermediate Prolog code generated by the compiler is
proportional to the size of the source code. Assuming that the
:ref:`term-expansion mechanism <expansion_expansion>` is not used, each
predicate clause in the source code is compiled into a single predicate
clause. But the Logtalk compiler also generates internal tables for the
defined entities, for the entity relations, and for the declared and
defined predicates. These tables enable support for fundamental features
such as :ref:`inheritance <inheritance_inheritance>` and
:ref:`reflection <reflection_reflection>`. The size of these tables is
proportional to the number of entities, entity relations, and predicate
declarations and definitions. When the :ref:`source_data <flag_source_data>`
is turned on (the default when *developing* an application), the generated
code also includes additional data about the source code such as entity and
predicates positions in a source file. This data enables advanced developer
tool functionality but it is usually not required when *deploying* an
application. Thus, turning this flag off is a common setting for minimizing
an application footprint.


Debug mode overhead
-------------------

Code compiled in debug mode runs slower, as expected, when compared with
normal or optimized mode. The overhead depends on the number of *debug events*
generated when running the application. A debug event is simply a pass on a
call or unification port of the :ref:`procedure box model <debugging_box_model>`.
These debug events can be intercepted by defined clauses for the
:ref:`logtalk::trace_event/2 <logtalk/0::trace_event/2>`
and :ref:`logtalk::debug_handler/2 <logtalk/0::debug_handler/2>` multifile
predicates. With no application (such as a debugger or a port profiler)
loaded defining clauses for these predicates, each goal have an overhead of
four extra inferences due to the runtime checking for a definition of the
hook predicates and a meta-call of the user goal. The clause head unification
events results in one or more inferences per goal (depending on the number of
clauses whose head unify with the goal and backtracking). In practice, this
overhead translates to code compiled in debug mode running typically ~2x to
~7x slower than code compiled in normal or optimized mode depending on the
application (the exact overhead is proportional to the number of passes on
the call and unification ports; deterministic code often results in a
relatively larger overhead when compared with code performing significant
backtracking).


Other considerations
--------------------

One aspect of performance, that affects both Logtalk and Prolog code, is
the characteristics of the Prolog VM. The Logtalk distribution includes
two examples,
`bench <https://github.com/LogtalkDotOrg/logtalk3/tree/master/examples/bench>`_
and
`benchmarks <https://github.com/LogtalkDotOrg/logtalk3/tree/master/examples/benchmarks>`_,
to help evaluate performance with specific backend Prolog systems. A
table with benchmark `results <https://logtalk.org/performance.html>`_ for
a subset of the supported systems is also available at the Logtalk
website. But note that multiple factors affect the performance of an
application and the benchmark examples and their results only provide
a partial assessment.
