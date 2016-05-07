________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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

This tool provides the default Logtalk command-line debugger.


API documentation
-----------------

To consult this tool API documentation, open in a web browser the file:

	docs/directory_index.html#tools/debugger/

For more information on using the debugger, open in a web browser the
following file  and consult the debugging section of the User Manual:

	manuals/index.html


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(debugger(loader)).

Note that this tool cannot be loaded at the same time as other tools (e.g.
the ports profiler) that also provide a debug handler, which must be unique
in a running session.


Usage
-----

Debugging Logtalk source code (with this debugger) requires compiling
source files using the `debug(on)` compiler flag. For example:

	| ?- logtalk_load(my_buggy_code, [debug(on)]).

In alternative, you may also turn on the `debug` flag globally by typing:

	| ?- set_logtalk_flag(debug, on).

Logtalk also provides basic support for the SWI-Prolog graphical tracer.
The required settings are described in the `settings-sample.lgt` file.

The `debugger_messages.lgt` source file defines the default debugger
message translations.

The `dump_trace.lgt` contains a simple code example on how to redirect
a goal trace to a file.


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

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
