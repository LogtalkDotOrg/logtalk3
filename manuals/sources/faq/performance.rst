
.. _performance:

Performance
===========

* :ref:`performance_meta_interpreter`
* :ref:`performance_code_generated`
* :ref:`performance_binding`
* :ref:`performance_prolog`

.. _performance_meta_interpreter:

Is Logtalk implemented as a meta-interpreter?
---------------------------------------------

No. Objects and their encapsulated predicates are compiled, not
meta-interpreted. In particular, inheritance relations are
pre-compiled for improved performance. Moreover, no meta-interpreter
is used even for objects compiled in debug mode.

.. _performance_code_generated:

What kind of code Logtalk generates when compiling objects? Dynamic code? Static code?
--------------------------------------------------------------------------------------

Static objects are compiled to static code. Static objects containing
dynamic predicates are also compiled to static code, except, of
course, for the dynamic predicates themselves. Dynamic objects are
necessarily compiled to dynamic code. As in Prolog programming, for
best performance, dynamic object predicates and dynamic objects
should only be used when truly needed.

.. _performance_binding:

How about message-sending performance? Does Logtalk use static binding or dynamic binding?
------------------------------------------------------------------------------------------

Logtalk supports both static binding and dynamic binding. When static
binding is not possible, Logtalk uses dynamic binding coupled with a
caching mechanism that avoids repeated lookups of predicate
declarations and predicate definitions. This is a solution common to
other programming languages supporting dynamic binding. Message
lookups are automatically cached the first time a message is sent.
Cache entries are automatically removed when loading entities or
using Logtalk dynamic features that invalidate the cached lookups.
Whenever static binding is used, message sending performance is
essentially the same as a predicate call in plain Prolog. Performance
of dynamic binding when lookups are cached is close to the
performance that would be achieved with static binding. See the
wiki section on `performance <https://github.com/LogtalkDotOrg/logtalk3/wiki/Performance>`__
for more details.

Which Prolog-dependent factors are most crucial for good Logtalk performance?
-----------------------------------------------------------------------------

Logtalk compiles objects assuming first-argument indexing for static
code. First-argument indexing of dynamic code, when available, helps
to improve performance due to the automatic caching of method lookups
and the necessary use of book-keeping tables by the runtime engine
(this is specially important when using event-driven programming).
Dynamic objects and static objects containing dynamic predicates also
benefit from first-argument indexing of dynamic predicates. The
availability of multi-argument indexing, notably for dynamic
predicates, also benefits dynamic binding performance.

.. _performance_prolog:

How does Logtalk performance compare with plain Prolog and with Prolog modules?
-------------------------------------------------------------------------------

Plain Prolog, Prolog modules, and Logtalk objects provide different
trade-offs between performance and features. In general, for a given
predicate definition, the best performance will be attained using
plain Prolog, second will be Prolog modules (assuming no explicitly
qualified calls are used), and finally Logtalk objects. Whenever
static binding is used, the performance of Logtalk is equal or close
to that of plain Prolog (depending on the Prolog virtual machine
implementation and compiler optimizations). See the
`simple benchmark test results <https://logtalk.org/performance.html>`_ using some
popular Prolog compilers.
