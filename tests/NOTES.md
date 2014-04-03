________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


This directory contains Logtalk unit tests for built-in entities, predicates,
control constructs, directives, and methods. Besides this set of unit tests,
there are also unit tests defined for most of the examples and for some of
the code contributions. Some of tests, specially for directives, are just
parsing tests for all the supported syntaxes.

You can automate running all these tests by calling the `logtalk_tester`
shell script from the command-line (see the `scripts/NOTES.md` file for
details on how to use with POSIX and Windows operating-systems). Type `man
logtalk_tester` or `logtalk_tester -h` for usage details, including how to
select the back-end Prolog compiler.

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
