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

This folder provides a simple tool for (re)generating documentation for a
project. The tool provides a `doclet` object that is expected to be extended
by the user to specify a sequence of goals and a sequence of shell commands
to (re)generate documentation. For usage examples see the `sample_doclet.lgt`
and `doclet1.lgt` source files.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(doclet(loader)).


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
