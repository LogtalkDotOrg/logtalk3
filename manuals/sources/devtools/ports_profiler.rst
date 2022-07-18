.. _library_ports_profiler:

``ports_profiler``
==================

This tool counts and reports the number of times each port in the
*procedure box model* is traversed during the execution of queries. It
can also report the number of times each clause (or grammar rule) is
used. It is inspired by the ECLiPSe ``port_profiler`` tool.

The procedure box model is the same used in the debugger tool. This is
an extended version of the original Byrd's four port model. Besides the
standard ``call``, ``exit``, ``fail``, and ``redo`` ports, Logtalk also
defines two post-unification ports, ``fact`` and ``rule``, and an
``exception`` port. This tool can also distinguishes between
deterministic exits (reported in the ``exit`` column in the profiling
result tables) and exits that leave choice-points (reported in the
``*exit`` column).

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#ports-profiler <../../docs/library_index.html#ports-profiler>`__

For sample queries, please see the ``SCRIPT.txt`` file in the tool
directory.

Loading
-------

::

   | ?- logtalk_load(ports_profiler(loader)).

Note that this tool cannot be loaded at the same time as other tools
(e.g. the debugger) that also provide a debug handler, which must be
unique in a running session.

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ports_profiler(tester)).

Compiling source files for port profiling
-----------------------------------------

To compile source files for port profiling, simply compile them in debug
mode and with the ``source_data`` flag turned on. For example:

::

   | ?- logtalk_load(my_source_file, [debug(on), source_data(on)]).

Alternatively, you can also simply turn on the ``debug`` and
``source_data`` flags globally before compiling your source files:

::

   | ?- set_logtalk_flag(debug, on), set_logtalk_flag(source_data, on).

Be aware, however, that loader files (e.g. library loader files) may
override default flag values and thus loaded files may not be compiled
in debug mode. In this case, you will need to modify the loader files
themselves.

Generating profiling data
-------------------------

After loading this tool and compiling the source files that you want to
profile in debug mode, simply call the goals to be profiled.

Printing profiling data reports
-------------------------------

After calling the goals that you want to profile, you can print a table
with all profile data by typing:

::

   | ?- ports_profiler::data.

To print a table with data for a single entity, use the query:

::

   | ?- ports_profiler::data(Entity).

To print a table with data for a single entity predicate, use the query:

::

   | ?- ports_profiler::data(Entity, Predicate).

In this case, the second argument must be either a predicate indicator,
``Name/Arity, or a non-terminal indicator,``\ Name//Arity`.

The profiling data can be reset using the query:

::

   | ?- ports_profiler::reset.

To reset only the data about a specific entity, use the query:

::

   | ?- ports_profiler::reset(Entity).

To illustrate the tool output, consider the ``family`` example in the
Logtalk distribution:

::

   | ?- {ports_profiler(loader)}.
   ...
   yes

   | ?- set_logtalk_flag(debug, on).
   yes

   | ?- logtalk_load(family(loader)).
   ...
   yes

   | ?- addams::sister(Sister, Sibling).
   Sister = wednesday,
   Sibling = pubert ;
   Sister = wednesday,
   Sibling = pugsley ;
   Sister = wednesday,
   Sibling = pubert ;
   Sister = wednesday,
   Sibling = pugsley ;
   no

   | ?- ports_profiler::data.
   ----------------------------------------------------------------------
   Entity     Predicate    Fact  Rule  Call  Exit *Exit  Fail  Redo Error
   ----------------------------------------------------------------------
   addams     female/1        2     0     1     1     1     0     1     0
   addams     parent/2        8     0     4     3     5     1     5     0
   relations  sister/2        0     1     1     0     4     1     4     0
   ----------------------------------------------------------------------
   yes

   | ?- ports_profiler::data(addams).
   -----------------------------------------------------------
   Predicate    Fact  Rule  Call  Exit *Exit  Fail  Redo Error
   -----------------------------------------------------------
   female/1        2     0     1     1     1     0     1     0
   parent/2        8     0     4     3     5     1     5     0
   -----------------------------------------------------------
   yes

   | ?- ports_profiler::data(addams, parent/2).
   -------------
   Clause  Count  
   -------------
        1      1
        2      1
        3      2
        4      1
        5      1
        6      2
   -------------
   yes

Interpreting profiling data
---------------------------

Some useful information that can be inferred from the profiling data
include:

-  which predicates are called more often (from the ``call`` port)
-  unexpected failures (from the ``fail`` port)
-  unwanted non-determinism (from the ``*exit`` port)
-  performance issues due to backtracking (from the ``*exit`` and
   ``redo`` ports)
-  predicates acting like a generator of possible solutions (from the
   ``*exit`` and ``redo`` ports)
-  inefficient indexing of predicate clauses (from the ``fact``,
   ``rule``, and ``call`` ports)
-  clauses that are never used or seldom used

The profiling data should be analyzed taking into account the expected
behavior for the profiled predicates.

Profiling Prolog modules
------------------------

This tool can also be applied to Prolog modules that Logtalk is able to
compile as objects. For example, if the Prolog module file is named
``module.pl``, try:

::

   | ?- logtalk_load(module, [debug(on), source_data(on)]).

Due to the lack of standardization of module systems and the abundance
of proprietary extensions, this solution is not expected to work for all
cases.

Profiling plain Prolog code
---------------------------

This tool can also be applied to plain Prolog code. For example, if the
Prolog file is named ``code.pl``, simply define an object including its
code and declaring as public any predicates that you want to use as
messages to the object. For example:

::

   :- object(code).

       :- public(foo/2).
       :- include('code.pl').

   :- end_object.

Save the object to an e.g. ``code.lgt`` file in the same directory as
the Prolog file and then load it in debug mode:

::

   | ?- logtalk_load(code, [debug(on), source_data(on)]).

In alternative, use the ``object_wrapper_hook`` provided by the
``hook_objects`` library:

::

   | ?- logtalk_load([os(loader), hook_objects(object_wrapper_hook)]).
   ...

   | ?- logtalk_load(
            code,
            [hook(object_wrapper_hook), debug(on),
             source_data(on), context_switching_calls(allow)]
        ).

In this second alternative, you can then use the ``(<<)/2`` context
switch control construct to call the wrapped predicates. E.g.

::

   | ?- code<<foo(X, Y).

With either wrapping solution, pay special attention to any compilation
warnings that may signal issues that could prevent the plain Prolog code
of working as-is when wrapped by an object. Often any required changes
are straight-forward (e.g. adding ``use_module/2`` directives for called
module library predicates).

Known issues
------------

Determinism information is currently not available when using Lean
Prolog or Quintus Prolog as backend compilers.
