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


This folder contains the default Logtalk documenting tool. It can
be loaded by typing:

	| ?- logtalk_load(lgtdoc(loader)).

Documenting Logtalk source code (with this tool) requires compiling
source files using the `source_data(on)` compiler flag. For example:

	| ?- logtalk_load(source_file, [source_data(on)]).

In alternative, you may also turn on the `source_data` flag globally
by typing:

	| ?- set_logtalk_flag(source_data, on).

This documenting tool uses Logtalk's structural reflection support
to extract and output in XML format relevant documentation about a
source code file, a library of source files, or all loaded source
files. The documenting predicates allows you to set several options
for the XML files, including the output directory. To consult the
documentation of the `lgtdoc` tool, open the `docs/tools.html` file
in a web browser. The `lgtdoc/xml` directory includes several ready
to use scripts for converting the XML documenting files to (X)HTML,
PDF, Markdown, or plain text files. See the `lgtdoc/xml/NOTES.md`
for details.

All source files are formatted using tabs (the recommended setting
is a tab width equivalent to 4 spaces).
