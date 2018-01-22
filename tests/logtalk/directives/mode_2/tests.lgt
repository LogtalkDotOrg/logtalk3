%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- set_logtalk_flag(source_data, on).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/08/03,
		comment is 'Unit tests for the mode/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(foo/6).
	:- mode(foo(+, -, ?, @, ++, --), zero).

	test(mode_2_1) :-
		predicate_property(foo(_,_,_,_,_,_), mode(Template, Solutions)),
		Template == foo(+, -, ?, @, ++, --),
		Solutions == zero.

	:- public(bar/6).
	:- mode(bar(+integer, -float, ?atom, @list, ++list, --stream), zero_or_one).

	test(mode_2_2) :-
		predicate_property(bar(_,_,_,_,_,_), mode(Template, Solutions)),
		Template == bar(+integer, -float, ?atom, @list, ++list, --stream),
		Solutions == zero_or_one.

	:- public(baz/1).
	:- mode(baz(@list(atom)), one).

	test(mode_2_3) :-
		predicate_property(baz(_), mode(Template, Solutions)),
		Template == baz(@list(atom)),
		Solutions == one.

	:- public(qux/1).
	:- mode(qux(-positive_integer), zero_or_more).

	test(mode_2_4) :-
		predicate_property(qux(_), mode(Template, Solutions)),
		Template == qux(-positive_integer),
		Solutions == zero_or_more.

	:- public(quux/1).
	:- mode(quux(+string), one_or_more).

	test(mode_2_5) :-
		predicate_property(quux(_), mode(Template, Solutions)),
		Template == quux(+string),
		Solutions == one_or_more.

:- end_object.
