________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>

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


This folder contains several useful programming tools. To load the main tools,
type:

	| ?- logtalk_load(tools(loader)).

The `ports` tool is not loaded by default, however, as it conflicts with
the `debugger` tool as both provide a debug handler that must be unique
in a running session.

The `profiler` tool is also not loaded by default as it provides integration
with selected backend Prolog compiler profilers that are not portable.

To load a specific tool, either change your Prolog working directory
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
- ports
- profiler

All source files are formatted using tabs (the recommended setting is a tab
width equivalent to 4 spaces).

Specific notes about each tool can be found in the corresponding `NOTES.md`
files. Basic XHTML documentation about each tool can be found on the `docs`
directory (open the `docs/tools.html` file with your web browser). To regenerate
this documentation, start Logtalk with your favorite back-end Prolog compiler
and follow these steps:

(1) If the `source_data` flag is not `on` by default, type the query:

	| ?- set_logtalk_flag(source_data, on).

(2) Load all the tools using the query:

	| ?- {tools(loader), ports(loader), profiler(loader)}.

(3) Generate the XML documenting files for all loaded tools using the query:

	| ?- lgtdoc::library(assertions, [xml_docs_directory('$LOGTALKUSER/docs/tmp_assertions')]),
		 lgtdoc::library(debugger, [xml_docs_directory('$LOGTALKUSER/docs/tmp_debugger')]),
		 lgtdoc::library(diagrams, [xml_docs_directory('$LOGTALKUSER/docs/tmp_diagrams')]),
		 lgtdoc::library(help, [xml_docs_directory('$LOGTALKUSER/docs/tmp_help')]),
		 lgtdoc::library(lgtdoc, [xml_docs_directory('$LOGTALKUSER/docs/tmp_lgtdoc')]),
		 lgtdoc::library(lgtunit, [xml_docs_directory('$LOGTALKUSER/docs/tmp_lgtunit')]),
		 lgtdoc::library(ports, [xml_docs_directory('$LOGTALKUSER/docs/tmp_ports')]),
		 lgtdoc::library(profiler, [xml_docs_directory('$LOGTALKUSER/docs/tmp_profiler')]).

(4) Run the command `lgt2html` on the temporary directories to generate the
(X)HTML documentation or the command `lgt2pdf` to generate PDF documentation:

	$ cd $LOGTALKUSER/docs/tmp_assertions && lgt2html -i assertions_tool.html -t "Assertions tool" && mv *.html ..
	$ cd ../tmp_debugger && lgt2html -i debugger_tool.html -t "Debugger tool" && mv *.html ..
	$ cd ../tmp_diagrams && lgt2html -i diagrams_tool.html -t "Diagrams tool" && mv *.html ..
	$ cd ../tmp_help && lgt2html -i help_tool.html -t "Help tool" && mv *.html ..
	$ cd ../tmp_lgtdoc && lgt2html -i lgtdoc_tool.html -t "Logtalk documenting tool" && mv *.html ..
	$ cd ../tmp_lgtunit && lgt2html -i lgtunit_tool.html -t "Logtalk unit testing tool" && mv *.html ..
	$ cd ../tmp_ports && lgt2html -i ports_tool.html -t "Port profiler tool" && mv *.html ..
	$ cd ../tmp_profiler && lgt2html -i profiler_tool.html -t "Profiler tool" && mv *.html ..

After generating the (X)HTML and/or PDF documentation, you can delete the
temporary directories:

	$ rm -rf $LOGTALKUSER/docs/tmp_*
