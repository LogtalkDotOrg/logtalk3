________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


This directory contains Logtalk unit tests for built-in entities, predicates,
control constructs, directives, and methods. Besides this set of unit tests,
there are also unit tests defined for most of the examples, for some of the
developer tools, and for some of the code contributions. Some tests, specially
for directives, are mainly parsing tests for all the supported syntaxes.

There's also an additional set of unit tests in this directory for Prolog
standard features. Most of these unit tests are taken from the official
ISO Prolog standard. They can be used to highlight differences between
backend Prolog compilers when porting and testing Logtalk applications.

You can automate running all these tests by calling the `logtalk_tester`
shell script from the command-line (see the `scripts/NOTES.md` file for
details on how to use this script with POSIX and Windows operating-systems).
Type `man logtalk_tester` or `logtalk_tester -h` for usage details, including
how to select the back-end Prolog compiler.

To run all the provided unit tests with e.g. SWI-Prolog as the back-end
compiler, open a terminal and type:

	$ cd $LOGTALKUSER/tests
	$ logtalk_tester -p swi
	...
	$ cd ../tools
	$ logtalk_tester -p swi
	...
	$ cd ../examples
	$ logtalk_tester -p swi
	...
	$ cd ../contributions
	$ logtalk_tester -p swi
	...

To run all the provided backend Prolog compiler standards conformance tests
with e.g. SWI-Prolog, open a terminal and type:

	$ cd $LOGTALKUSER/tests/prolog
	$ logtalk_tester -p swi
	...
