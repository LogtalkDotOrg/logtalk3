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


This folder contains the default Logtalk command-line debugger. It can
be loaded by typing:

	| ?- logtalk_load(debugger(loader)).

Debugging Logtalk source code (with this debugger) requires compiling
source files using the `debug(on)` compiler option. For example:

	| ?- logtalk_load(my_buggy_code, [debug(on)]).

In alternative, you may also turn on the `debug` flag globally by typing:

	| ?- set_logtalk_flag(debug, on).

Logtalk also provides basic support for the SWI-Prolog graphical tracer.
The required settings are described in the `settings-sample.lgt` file.

The `debugger_messages.lgt` source file defines the default debugger
message translations. For more information on the debugger entities,
open the `docs/tools.html` file in a web browser. For help using the
debugger, open the `manuals/index.html` file in a web browser and
consult the debugging section of the User Manual.

The `dump_trace.lgt` contains a simple code example on how to redirect
a goal trace to a file.

All source files are formatted using tabs (the recommended setting is
a tab width equivalent to 4 spaces).
