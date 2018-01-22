________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


Available developer tools
-------------------------

The following developer tools are available, each one with its own
`loader.lgt` loader file (except make which is a built-in tool) and
`NOTES.md` documentation files:

- assertions
- code_metrics
- dead_code_scanner
- debugger
- diagrams
- doclet
- help
- lgtdoc
- lgtunit
- make
- ports
- profiler
- wrapper


Loading the developer tools
---------------------------

To load the main developer tools, use the following goal:

	| ?- logtalk_load(tools(loader)).

The `ports` tool is not loaded by default, however, as it conflicts with
the `debugger` tool as both provide a debug handler that must be unique
in a running session.

The `profiler` tool is also not loaded by default as it provides integration
with selected backend Prolog compiler profilers that are not portable.

The `wrapper` tool is also not loaded by default given its beta status.

To load a specific tool, either change your Prolog working directory
to the tool folder and then compile and load the corresponding loader 
utility file or simply use library notation as argument for the 
compiling and loading predicates. For example:

	| ?- logtalk_load(lgtunit(loader)).


Tools documentation
-------------------

Specific notes about each tool can be found in the corresponding `NOTES.md`
files. Basic XHTML documentation about each tool can be found on the `docs`
directory (open the `docs/index.html` file with your web browser). The
documentation for these tools can be regenerated using the shell scripts
`../scripts/update_html_docs.sh` and `../scripts/update_svg_docs.sh`.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
