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

This folder provides a simple tool for (re)generating documentation for an
application. The tool provides a `doclet` object that is expected to be
extended by the user to specify a sequence of goals and a sequence of shell
commands that load the application and (re)generate its documentation.

Doclet source files are usually named `doclet.lgt` (or `doclet.logtalk`) and
the doclet object are usually named after the application or library to be
documented with a `_doclet` suffix. By using an `initialization/1` directive
to automatically send the `update/0` message that generates the documentation
upon doclet loading, we can abstract the name of the doclet object. The usual
query to load and run a doclet is therefore:

	| ?- logtalk_load([doclet(loader), doclet]).

For usage examples see the `sample_doclet.lgt`, `doclet1.lgt`,
`zoom_doclet.lgt`, and `tools_doclet.lgt` source files.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(doclet(loader)).


Automating running doclets
--------------------------

You can use the `scripts/logtalk_doclet.sh` Bash shell script for automating
running doclets. See the `scripts/NOTES.md` file for details.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
