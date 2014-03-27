
________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>

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


Overview
--------

The `assertions.lgt` file contains definitions for two meta-predicates,
`assertion/1-2`, which allows using of assertions on your source code
(e.g. when writing unit tests). The `assertions_messages.lgt` file defines
the default message translations generated on assertions succeed, fail, or
throw an exception. For more information on these entities, open the
`docs/tools.html` file in a web browser.


Adding assertions to your code
------------------------------

The `assertion/1` predicate takes a goal as argument. For example,
assuming that you're writing a unit test:

	test(assertions_1) :-
		assertions::assertion(ground(x)),
		2 is 1 + 1.

The `assertion/2` predicate takes as arguments a term for passing
context information and a goal. Using again a unit test as an example:

	test(assertions_1) :-
		assertions::assertion(assertions_1, ground(x)),
		2 is 1 + 1.

When using a large number of assertions, you can use a lighter syntax
by adding a `uses/2` directive:

	:- uses(assertions, [assertion/1, assertion/2]).


Intercepting assertion messages
-------------------------------

By default, assertion failures and errors are printed to the standard
output stream. These messages, however, can be intercepted by defining
the `logtalk::message_hook/4` multifile predicate. For example:

	:- category(my_assertions_settings).
	
		:- initialization(open('bugs.lgt', append, _, [alias(bugs)])).
	
		:- multifile(logtalk::message_hook/4).
		:- dynamic(logtalk::message_hook/4).
	
		logtalk::message_hook(assertion_failure(Context, Goal), _, assertions, _) :-
			writeq(bugs, assertion_failure(Context, Goal)), write('.\n').
	
	:- end_category.


Converting assertion failures into errors
-----------------------------------------

	:- category(assertions_to_errors).
	
		:- multifile(logtalk::message_hook/4).
		:- dynamic(logtalk::message_hook/4).
	
		logtalk::message_hook(assertion_failure(Context, Goal), _, assertions, Tokens) :-
			logtalk::message_prefix_stream(error, assertions, Prefix, Stream),
			logtalk::print_message_tokens(Stream, Prefix, Tokens),
			throw(error(assertion_failure(Goal), Context)).
	
	:- end_category.
