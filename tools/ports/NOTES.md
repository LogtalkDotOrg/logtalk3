
________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Additional licensing terms apply per Section 7 of the GNU General
Public License 3. Consult the `LICENSE.txt` file for details.
________________________________________________________________________


Overview
--------

The `ports.lgt` source file defines a simple predicate execution box model
port profiler tool (inspired by the ECLiPSe `port_profiler` tool). The box
model is the same used in the debugger tool. This tool can be loaded using
the query:

	| ?- logtalk_load(ports(loader)).

The Logtalk predicate execution box model is an extended version of the
original Byrd’s four port model. Besides the `call`, `exit`, `fail`, and
`redo` ports, Logtalk also defines two (post-)unification ports, `fact`
and `rule`, and an `exception` port. This tool counts and reports the
number of times each port is traversed during the execution of queries.
It also distinguishes between deterministic exits (reported in the `exit`
column in the profiling result tables) and exits that leave choice-points
(reported in the `*exit` column).

All source files are formatted using tabs (the recommended setting is a tab
width equivalent to 4 spaces).


Compiling source files for port profiling
-----------------------------------------

To compile source files for port profiling, simply compile them in debug mode
and with the `source_data` flag turned on.
For example:

	| ?- logtalk_load(my_source_file, [debug(on), source_data(on)]).

You can also simply turn on the `debug` and `source_data` flags globally before
compiling your source files:

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

To reset only the data about a single entity, use the query:

	| ?- ports::reset(Entity).
 