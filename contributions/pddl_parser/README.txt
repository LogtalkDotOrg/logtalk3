================================================================
PDDL 3.0 parser
Release 1.0 

Copyright (c) 2011 Robert Sasak.            All Rights Reserved.
This is free software.     You can redistribute it and/or modify
it under the terms of the "Artistic License 2.0" as published by 
The Perl Foundation. Consult the "LICENSE.txt" file for details.
================================================================


This PDDL 3.0 file parser converts PDDL files to Logtalk/Prolog
friendly syntax. For example:

	PDDL         Prolog 
	(on ?x ?y)   on(?x, ?y)

Syntax sugar:
	op(200, fy, ?).

Usage:
	The PDDL parser can be loaded from its directory by typing:
		?- logtalk_load(loader).
	Or from any directory by typing:
		?- logtalk_load(pddl_parser(loader)).

For whole example check the "pddl.lgt" file for usage and example
output.

Validation:
	The provided unit tests are based on a collection of problemset
	files from International Planning Competition 2008. in order to
	run all unit tests from the parser's directory type:
		?- logtalk_load(tester).
	Some of the unit tests fail in some Prolog compilers due to
	limitations to the maximum arity of a term.
