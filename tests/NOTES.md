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


This directory contains two test suites. The Logtalk test suite includes unit
tests for built-in entities, predicates, control constructs, directives, and
methods. Besides this set of unit tests, there are also unit tests defined for
most of the examples, for some of the developer tools, and for some of the code
contributions. Some tests, specially for directives, are mainly parsing tests
for all the supported syntaxes.

The Prolog test suite includes conformance tests for both official and de
facto standards. It can be used to highlight differences between backend
Prolog compilers when porting and testing Logtalk applications. See the
`prolog/NOTES.md` file for details about the these tests and how to run
them.

You can automate running all these tests by calling the `logtalk_tester`
shell script from the command-line (see the `scripts/NOTES.md` file for
details on how to use this script with POSIX and Windows operating-systems).
Type `man logtalk_tester` or `logtalk_tester -h` for usage details, including
how to select the back-end Prolog compiler.

To run all the provided unit tests with e.g. SWI-Prolog as the back-end
compiler, open a terminal and type:

	$ cd $LOGTALKUSER/tests/logtalk
	$ logtalk_tester -p swi
	...
	$ cd $LOGTALKUSER/tests/prolog
	$ logtalk_tester -p swi
	...
	$ cd $LOGTALKUSER/tools
	$ logtalk_tester -p swi
	...
	$ cd $LOGTALKUSER/examples
	$ logtalk_tester -p swi
	...
	$ cd $LOGTALKUSER/contributions
	$ logtalk_tester -p swi
	...
