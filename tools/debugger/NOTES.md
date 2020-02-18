________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


`debugger`
==========

This tool provides the default Logtalk command-line debugger.


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#debugger](../../docs/library_index.html#debugger)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(debugger(loader)).

Note that this tool cannot be loaded at the same time as other tools (e.g.
the ports profiler) that also provide a debug handler, which must be unique
in a running session.

When the code to be debugged runs computationally expensive initializations,
loading this tool after the code may have a noticeable impact in loading
times.


Usage
-----

Debugging Logtalk source code (with this debugger) requires compiling
source files using the `debug(on)` compiler flag. For example:

	| ?- logtalk_load(my_buggy_code, [debug(on)]).

In alternative, you may also turn on the `debug` flag globally by typing:

	| ?- set_logtalk_flag(debug, on).

But note that loader files may override this flag setting (e.g. by using
`debug(off)` or `optimize(on)` options for loaded files). If that's the
case, you will need to either edit the loader files or write customized
loader files enabling debugging. For detailed information on using the
debugger, consult the debugging section of the User Manual:

[../../manuals/userman/debugging.html](../../manuals/userman/debugging.html)

The `debugger_messages.lgt` source file defines the default debugger
message translations.

The `dump_trace.lgt` contains a simple code example on how to redirect
a goal trace to a file.


Alternative debugger tools
--------------------------

Logtalk provides basic support for the SWI-Prolog graphical tracer. The
**required** settings are described in the `settings-sample.lgt` file. Logtalk
queries can be traced using this tool by using the `gtrace/0-1` predicates.
For example:

	 | ?- gtrace(foo::bar).

or:

	 | ?- gtrace, foo::bar.

You can also use the `gspy/1` predicate to spy a Logtalk predicate specified
as `Entity::Functor/Arity` when using the graphical tracer. When using this
tool, internal Logtalk compiler/runtime predicates and compiled predicates
that resulted from the term-expansion mechanism may be exposed in some cases.
This issue is shared with Prolog and results from the non-availability of
source code for the predicates being traced.


Known issues
------------

Line number spy points require a Prolog backend compiler that supports
accessing read term starting line but only some systems (B-Prolog, JIProlog,
Lean Prolog, SWI-Prolog, and YAP) provide accurate line numbers.

As a workaround, you can check the start line number for an entity predicate
definition using a query such as:

	| ?- object_property(Entity, defines(Functor/Arity, Properties)).

and checking the returned `line_count/1` property to find if there's any
offset to the source file number of the predicate clause that you want to
trace. This issue, if present, usually only affects the first predicate
clause.

Line number spy points are currently not available when using XSB as the
Prolog backend compiler.


Other notes
-----------

All source files are indented using tabs (a common setting is a tab width
equivalent to 4 spaces).
