________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>

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


This folder contains a single Prolog file, `core.pl`, which implements
the Logtalk compiler and the Logtalk runtime. There are also some Logtalk
source files defining built-in protocols, categories, and objects:

- `expanding.lgt`  
	built-in `expanding` protocol specifying term- and goal-expansion predicates
- `forwarding.lgt`  
	built-in `forwarding` protocol specifying the message forwarding predicate
- `monitoring.lgt`  
	built-in `monitoring` protocol specifying the event handler predicates
- `logtalk.lgt`  
	built-in `logtalk` object defining message printing, debugging, and hacking predicates
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
	| ?- lgtdoc::library(core, [xmldir('$LOGTALKUSER/docs/tmp')]).

(3) Run the command `lgt2html` on the `$LOGTALKUSER/docs/tmp` directory
to generate (X)HTML documentation or the command `lgt2pdf` to generate PDF
documentation. For example:

	$ cd $LOGTALKUSER/docs/tmp
	$ lgt2html -i core.html -t "Core entities documentation index" && mv *.html ..

After generating the (X)HTML and/or PDF documentation, you can delete the
temporary directories:

	$ rm -rf $LOGTALKUSER/docs/tmp_*
