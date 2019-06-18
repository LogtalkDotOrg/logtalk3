________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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


`lgtdoc`
========

This folder provides the default Logtalk documenting tool, which is focused on
generating API documentation for applications.

This documenting tool uses Logtalk's structural reflection support to extract
and output in XML format relevant documentation about a source code file, a
library or directory of source files, or all loaded source files. The tool
predicates allows you to set several options for the XML files, including the
output directory.

The `lgtdoc/xml` directory includes several ready to use scripts for converting
the XML documenting files into final formats including (X)HTML, PDF, Markdown,
and reStructuredText (for use with Sphinx), or plain text files. The scripts are
described in their `man` pages. See also the `lgtdoc/xml/NOTES.md` for details.


For information on documenting your source code, consult the documenting section
of the User Manual:

[manuals/userman/documenting.html](https://logtalk.org/manuals/userman/documenting.html)



API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/library_index.html#lgtdoc](https://logtalk.org/docs/library_index.html#lgtdoc)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(lgtdoc(loader)).


Documenting source code
-----------------------

Documenting Logtalk source code (with this tool) requires compiling the source
files using the `source_data(on)` compiler flag. For example:

	| ?- logtalk_load(source_file, [source_data(on)]).

Usually, this flag is set for all application source files in the corresponding
loader file. In alternative, you may also turn on the `source_data` flag
globally by typing:

	| ?- set_logtalk_flag(source_data, on).

The tool API allows generating documentation for libraries, directories, and
files, complemented with library, directory, entity, and predicate indexes.
The source files to be documented **must** be loaded prior to using this tool
predicates to generate the documentation.


Generating documentation
------------------------

For a simple application, assuming a library alias is defined for it (e.g.
`my_app`), and at the top-level interpreter, we can generate the application
documentation by typing:

	| ?- {my_app(loader)}.
	...
	
	| ?- {lgtdoc(loader)}.
	...
	
	| ?- lgtdoc::library(my_app).
	...

By default, the documenting XML files are created in a `./xml_docs` directory
in the application directory. For example, assuming that we want to generate
HTML documentation:

	$ cd xml_docs
	$ lgt2html -t "My app"

For more complex applications, you can use the `doclet` tool to define a *doclet*
to automate all the steps required to generate documentation. The *doclet* message
that triggers the process can also be sent automatically when the `make` tool is
used. 


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
