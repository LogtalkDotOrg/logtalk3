%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paul Brown <pbrown@optimusprime.ai> and
%                      Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% predicate for testing callbacks
content(strong('Hello world!')).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:3:1,
		author is 'Paulo Moura',
		date is 2021-11-25,
		comment is 'Unit tests for the "html" library.'
	]).

	cover(html).
	cover(html5).
	cover(xhtml11).

	test(html_01, error(instantiation_error)) :-
		html5::generate(_, []).

	test(html_02, error(instantiation_error)) :-
		html5::generate(file(_), []).

	test(html_03, error(instantiation_error)) :-
		html5::generate(stream(_), []).

	test(html_04, error(instantiation_error)) :-
		html5::generate(file('foo.html'), _).

	test(html_05, error(instantiation_error)) :-
		current_output(Stream),
		html5::generate(stream(Stream), [a| _]).

	test(html_06, error(domain_error(html_sink, foo))) :-
		html5::generate(foo, [a]).

	test(html_07, error(domain_error(html_element, bar(a)))) :-
		html5::generate(file('foo.html'), bar(a)).

	test(html_08, true) :-
		^^suppress_text_output,
		current_output(Stream),
		html5::generate(stream(Stream), html([lang=en], [head(title('Hello world!')), body(p('Bye!'))])).

	test(html_09, true(Assertion)) :-
		^^set_text_output(''),
		current_output(Stream),
		html5::generate(stream(Stream), code([foo,bar,baz])),
		^^text_output_assertion('<code>[foo,bar,baz]</code>', Assertion).

	test(html_10, true) :-
		^^suppress_text_output,
		current_output(Stream),
		xhtml11::generate(stream(Stream), html([lang=en], [head(title('Hello world!')), body(p('Bye!'))])).

	test(html_11, true(Assertion)) :-
		^^set_text_output(''),
		current_output(Stream),
		xhtml11::generate(stream(Stream), code([foo,bar,baz])),
		^^text_output_assertion('<code>[foo,bar,baz]</code>', Assertion).

	test(html_12, true) :-
		^^suppress_text_output,
		create_object(
			Custom,
			[extends(html5)],
			[],
			[
				normal_element(foo,inline),
				(normal_element(Name, Display) :- ^^normal_element(Name, Display))
			]
		),
		current_output(Stream),
		Custom::generate(stream(Stream), foo(bar)).

	test(html_13, true(Assertion)) :-
		^^set_text_output(''),
		current_output(Stream),
		html5::generate(stream(Stream), span(user::content)),
		^^text_output_assertion('<span><strong>Hello world!</strong></span>', Assertion).

	cleanup :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'foo.html', File),
		catch(ignore(os::delete_file(File)), _, true).

:- end_object.
