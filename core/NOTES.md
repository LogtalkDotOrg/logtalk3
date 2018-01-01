________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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


This folder contains a single Prolog file, `core.pl`, which implements the
Logtalk compiler and runtime. There are also several Logtalk source files
defining built-in protocols, categories, and objects:

- `expanding.lgt`  
	built-in `expanding` protocol specifying term- and goal-expansion predicates
- `forwarding.lgt`  
	built-in `forwarding` protocol specifying the message forwarding predicate
- `monitoring.lgt`  
	built-in `monitoring` protocol specifying the event handler predicates
- `logtalk.lgt`  
	built-in `logtalk` object defining message printing, question asking, debugging, and hacking predicates
- `core_messages.lgt`  
	built-in `core_messages` category defining the default translations for compiler messages
- `user.lgt`  
	definition of the built-in pseudo-object `user`

Before loading the `core.pl` file into your favorite Prolog compiler,
you must first load the appropriated adapter file for your Prolog
compiler, which you will find in the `adapters` directory, and the
`paths/paths.pl` file, which defines essential library paths for
starting Logtalk. The provided Prolog POSIX integration scripts and
Windows shortcuts automate this process and should be used unless
there's a strong reason to manually load Logtalk.

The source files are formatted using tabs (the recommended setting is
a tab width equivalent to 4 spaces).

The `$LOGTALKUSER/docs` directory includes a XHTML version of the core
entities documentation. To regenerate the documentation of these libraries,
start Logtalk with your favorite back-end Prolog compiler and follow these
steps:

(1) If the `source_data` flag is not turned `on` by default, type the query:

	| ?- set_logtalk_flag(source_data, on).

(2) Load the `lgtdoc` tool and generate the XML documenting files for all
core entities using the queries:

	| ?- {lgtdoc(loader)}.
	...
	| ?- lgtdoc::library(core, [xml_docs_directory('$LOGTALKUSER/docs/tmp')]).

(3) Run the command `lgt2html` on the `$LOGTALKUSER/docs/tmp` directory
to generate (X)HTML documentation or the command `lgt2pdf` to generate PDF
documentation. For example:

	$ cd $LOGTALKUSER/docs/tmp
	$ lgt2html -i core.html -t "Core entities documentation index" && mv *.html ..

After generating the (X)HTML and/or PDF documentation, you can delete the
temporary directories:

	$ rm -rf $LOGTALKUSER/docs/tmp_*
