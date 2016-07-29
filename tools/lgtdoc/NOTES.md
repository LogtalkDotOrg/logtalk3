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

This folder provides the default Logtalk documenting tool, which is focused on
generating API documentation for applications.

This documenting tool uses Logtalk's structural reflection support to extract
and output in XML format relevant documentation about a source code file, a
library or directory  of source files, or all loaded source files. The tool
predicates allows you to set several options for the XML files, including the
output directory.


API documentation
-----------------

To consult this tool API documentation, open in a web browser the file:

[docs/directory_index.html#tools/lgtdoc/](http://logtalk.org/docs/directory_index.html#tools/lgtdoc/)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(lgtdoc(loader)).


Documenting source code
-----------------------

Documenting Logtalk source code (with this tool) requires compiling the source
files using the `source_data(on)` compiler flag. For example:

	| ?- logtalk_load(source_file, [source_data(on)]).

In alternative, you may also turn on the `source_data` flag globally by typing:

	| ?- set_logtalk_flag(source_data, on).

The tool API allows generating documentation for libraries, directories, and
files, complemented with directory, entity, and predicate indexes.

The `lgtdoc/xml` directory includes several ready to use scripts for converting
the XML documenting files to (X)HTML, PDF, Markdown, or plain text files. See
the `lgtdoc/xml/NOTES.md` for details.

An utility `doclet` object is available providing a simple solution to specify a
set of goals and a set of shell commands to (re)generate the API documentation
for a project. For an usage example see the `sample_doclet.lgt` source file.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
