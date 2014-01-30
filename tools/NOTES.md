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


This folder contains some useful programming tools. To load all tools,
type:

	| ?- logtalk_load(tools(loader)).

To load a specific tool either change your Prolog working directory
to the tool folder and then compile and load the corresponding loader 
utility file or simply use library notation as argument for the 
compiling and loading predicates. For example:

	| ?- logtalk_load(lgtunit(loader)).

Currently, the following tools are available, each one with its own
`loader.lgt` and `NOTES.md` files:

- assertions
- debugger
- diagrams
- help
- lgtdoc
- lgtunit
- profiler

All source files are formatted using tabs (the recommended setting is a tab
width equivalent to 4 spaces).

Specific notes about each tool can be found in the corresponding `NOTES.md`
files. Basic XHTML documentation about each tool can be found on the `docs`
directory (open the `tools.html` file in your web browser). To regenerate
this documentation, start Logtalk with your favorite back-end Prolog compiler
and follow these steps:

(1) If the `source_data` flag is not `on` by default, type the query:

	| ?- set_logtalk_flag(source_data, on).

(2) Load all the tools using the query:

	| ?- {tools(loader)}.

(3) Generate the XML documenting files for all loaded tools using the query:

	| ?- lgtdoc::library(assertions, [xmldir('$LOGTALKUSER/docs/tmp_assertions')]),
		lgtdoc::library(debugger, [xmldir('$LOGTALKUSER/docs/tmp_debugger')]),
		lgtdoc::library(diagrams, [xmldir('$LOGTALKUSER/docs/tmp_diagrams')]),
		lgtdoc::library(help, [xmldir('$LOGTALKUSER/docs/tmp_help')]),
		lgtdoc::library(lgtdoc, [xmldir('$LOGTALKUSER/docs/tmp_lgtdoc')]),
		lgtdoc::library(lgtunit, [xmldir('$LOGTALKUSER/docs/tmp_lgtunit')]),
		lgtdoc::library(profiler, [xmldir('$LOGTALKUSER/docs/tmp_profiler')]).

(4) Run the command `lgt2html` on the temporary directories to generate the
(X)HTML documentation or the command `lgt2pdf` to generate PDF documentation:

	$ cd $LOGTALKUSER/docs/tmp_assertions && lgt2html -i assertions_tool.html -t "Assertions" && mv *.html ..
	$ cd ../tmp_debugger && lgt2html -i debugger_tool.html -t "Debugger" && mv *.html ..
	$ cd ../tmp_diagrams && lgt2html -i diagrams_tool.html -t "Diagrams" && mv *.html ..
	$ cd ../tmp_help && lgt2html -i help_tool.html -t "Help" && mv *.html ..
	$ cd ../tmp_lgtdoc && lgt2html -i lgtdoc_tool.html -t "LgtDoc" && mv *.html ..
	$ cd ../tmp_lgtunit && lgt2html -i lgtunit_tool.html -t "LgtUnit" && mv *.html ..
	$ cd ../tmp_profiler && lgt2html -i profiler_tool.html -t "Profiler" && mv *.html ..

After generating the (X)HTML and/or PDF documentation, you can delete the
temporary directories:

	$ rm -rf $LOGTALKUSER/docs/tmp_*
