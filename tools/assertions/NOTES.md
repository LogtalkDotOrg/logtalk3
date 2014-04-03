
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


Adding assertions to your source code
-------------------------------------

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
by adding a `uses/2` directive. For example:

	:- uses(assertions, [assertion/1, assertion/2]).


Automatically adding file and line context information to assertions
--------------------------------------------------------------------

The `assertions/1` parametric object can be used as a hook object to
automatically add file and line context information, represented by the
term `file_lines(File, BeginLine-EndLine)`, to calls to the `assertion/1`
predicate by goal-expanding it to calls to the `assertion/2` predicate
(the expansion assumes that a `uses/2` directive is being used in the code
that will be expanded to direct `assertion/1` calls to the `assertions`
object). For example, assuming the file using assertions is named `source`,
it would be compiled and loaded using the call:

	logtalk_load(source, [hook(assertions(debug))])


Suppressing assertion calls from source code
--------------------------------------------

The `assertions/1` parametric object can be used as a hook object to
suppress calls to the `assertion/1-2` predicates using goal-expansion
(the expansion assumes that a `uses/2` directive is being used in the
code that will be expanded to direct `assertion/1-2` calls to the
`assertions` object). For example, assuming the file using assertions
is named `source`, it would be compiled and loaded using the call:

	logtalk_load(source, [hook(assertions(production))])


Redirecting assertion failure messages
--------------------------------------

By default, assertion failures and errors are printed to the standard
output stream. These messages, however, can be intercepted by defining
the `logtalk::message_hook/4` multifile predicate. For example:

	:- category(redirect_assertions_messages(_File)).
	
		:- multifile(logtalk::message_hook/4).
		:- dynamic(logtalk::message_hook/4).
	
		logtalk::message_hook(Message, error, assertions, _) :-
			parameter(1, File),
			writeq(File, Message), write(File, '.'), nl(File).
	
	:- end_category.


Converting assertion failures into errors
-----------------------------------------

If you want an assertion failure to result in a runtime error, you can
intercept the assertion failure messages, optionally still printing them,
and throw an error. For example:

	:- category(assertions_failures_to_errors).
	
		:- multifile(logtalk::message_hook/4).
		:- dynamic(logtalk::message_hook/4).
	
		logtalk::message_hook(Message, error, assertions, Tokens) :-
			% uncomment the next two lines to also print the default assertion failure message
			% logtalk::message_prefix_stream(error, assertions, Prefix, Stream),
			% logtalk::print_message_tokens(Stream, Prefix, Tokens),
			throw(error(Message, _)).
	
	:- end_category.
