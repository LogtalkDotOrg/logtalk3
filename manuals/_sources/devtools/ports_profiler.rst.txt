``ports_profiler``
==================

The ``ports_profiler.lgt`` source file defines a simple predicate
execution box model port profiler tool (inspired by the ECLiPSe
``port_profiler`` tool). The box model is the same used in the debugger
tool.

The Logtalk predicate execution box model is an extended version of the
original Byrd's four port model. Besides the standard ``call``,
``exit``, ``fail``, and ``redo`` ports, Logtalk also defines two
(post-)unification ports, ``fact`` and ``rule``, and an ``exception``
port. This tool counts and reports the number of times each port is
traversed during the execution of queries. It also distinguishes between
deterministic exits (reported in the ``exit`` column in the profiling
result tables) and exits that leave choice-points (reported in the
``*exit`` column).

API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

`docs/library_index.html#ports-profiler <https://logtalk.org/docs/library_index.html#ports-profiler>`__

For sample queries, please see the `SCRIPT.txt <SCRIPT.txt>`__ file in
the tool directory.

Loading
-------

::

   | ?- logtalk_load(ports_profiler(loader)).

Note that this tool cannot be loaded at the same time as other tools
(e.g. the debugger) that also provide a debug handler, which must be
unique in a running session.

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

The profiling data can be reset using the query:

::

   | ?- ports_profiler::reset.

To reset only the data about a specific entity, use the query:

::

   | ?- ports_profiler::reset(Entity).

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

The profiling data should be analyzed taking into account the expected
behavior for the profiled predicates.

Known issues
------------

Determinism information is currently not available when using Lean
Prolog or Quintus Prolog as backend compilers.

Other notes
-----------

All source files are indented using tabs (a common setting is a tab
width equivalent to 4 spaces).
