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


This folder contains the default Logtalk documenting tool. It can
be loaded by typing:

	| ?- logtalk_load(lgtdoc(loader)).

Documenting Logtalk source code (with this tool) requires compiling
source files using the `source_data(on)` compiler option. For a single
source file, you can type, for example:

	| ?- logtalk_load(source_file, [source_data(on)]).

For multiple source files, assuming you use a loader file, you can set
the `source_data` flag in the loader's `initialization/1` directive:

	:- initialization((
		set_logtalk_flag(source_data, on),
		logtalk_load(source_file_1),
		...
	)).

In alternative, you may also turn on the `source_data` flag globally by
typing:

	| ?- set_logtalk_flag(source_data, on).

This documenting tool uses Logtalk's structural reflection support
to extract and output in XML format relevant documentation about a
source code file, a library of source files, or all loaded source
files. The documenting predicates allows you to set several options
for the XML files, including the output directory. To consult the
documentation of the `lgtdoc` tool, open the `docs/tools.html` file
in a web browser. The `lgtdoc/xml` directory includes several ready
to use scripts for converting the XML documenting files to e.g. PDF
or (X)HTML. See the `lgtdoc/xml/NOTES.md` for details.

All source files are formatted using tabs (the recommended setting
is a tab width equivalent to 4 spaces).
