
________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


Overview
--------

The `ports.lgt` source file defines a simple predicate execution box model
port profiler tool (inspired by the ECLiPSe `port_profiler` tool). The box
model is the same used in the debugger tool.

The Logtalk predicate execution box model is an extended version of the
original Byrd's four port model. Besides the standard `call`, `exit`, `fail`,
and `redo` ports, Logtalk also defines two (post-)unification ports, `fact`
and `rule`, and an `exception` port. This tool counts and reports the
number of times each port is traversed during the execution of queries.
It also distinguishes between deterministic exits (reported in the `exit`
column in the profiling result tables) and exits that leave choice-points
(reported in the `*exit` column).


API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/directory_index.html#tools/ports/](http://logtalk.org/docs/directory_index.html#tools/ports/)


Loading
-------

	| ?- logtalk_load(ports(loader)).

Note that this tool cannot be loaded at the same time as other tools (e.g.
the debugger) that also provide a debug handler, which must be unique in a
running session.


Compiling source files for port profiling
-----------------------------------------

To compile source files for port profiling, simply compile them in debug mode
and with the `source_data` flag turned on. For example:

	| ?- logtalk_load(my_source_file, [debug(on), source_data(on)]).

Alternatively, you can also simply turn on the `debug` and `source_data` flags
globally before compiling your source files:

	| ?- set_logtalk_flag(debug, on), set_logtalk_flag(source_data, on).


Generating profiling data
-------------------------

After loading this tool and compiling the source files that you want to profile
in debug mode, simply call the goals to be profiled.


Printing profiling data reports
-------------------------------

After calling the goals that you want to profile, you can print a table with 
all profile data by typing:

	| ?- ports::data.

To print a table with data for a single entity, use the query:

	| ?- ports::data(Entity).

The profiling data can be reset using the query:

	| ?- ports::reset.

To reset only the data about a specific entity, use the query:

	| ?- ports::reset(Entity).


Interpreting profiling data
---------------------------

Some of the useful information that can be inferred from the profiling data
include:

- which predicates are called more often (from the `call` port)
- unexpected failures (from the `fail` port)
- unwanted non-determinism (from the `*exit` port)
- performance issues due to backtracking (from the `*exit` and `redo` ports)
- predicates acting like a generator of possible solutions (from the `*exit` and `redo` ports)
- inefficient indexing of predicate clauses (from the `fact`, `rule`, and `call` ports)

The profiling data should be analyzed taking into account the expected
behavior for the profiled predicates.


Known issues
------------

Determinism information is currently not available when using Lean Prolog
or Quintus Prolog as backend compilers.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
