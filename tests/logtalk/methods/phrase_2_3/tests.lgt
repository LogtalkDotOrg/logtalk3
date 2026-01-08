%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2023-12-04,
		comment is 'Unit tests for the phrase/2-3 built-in methods.'
	]).

	% linter warnings are avoided and errors are delayed to runtime
	% by using the {}/1 compiler bypass control construct

	% phrase/2 tests

	test(phrase_2_01, error(permission_error(access, private_predicate, phrase/2))) :-
		{logtalk::phrase(_, _)}.

	test(phrase_2_02, error(instantiation_error)) :-
		{logtalk << phrase(_, _)}.

	test(phrase_2_03, error(type_error(callable,1))) :-
		{logtalk << phrase(1, _)}.

	test(phrase_2_05, error(type_error(list,1))) :-
		{logtalk << phrase(foo, 1)}.

	test(phrase_2_06, error(type_error(list,[1|2]))) :-
		{logtalk << phrase(foo, [1|2])}.

	test(phrase_2_07, error(existence_error(procedure,foo/2))) :-
		{logtalk << phrase(foo, _)}.

	% phrase/3 tests

	test(phrase_3_01, error(permission_error(access, private_predicate, phrase/3))) :-
		% avoid linter warnings by using the {}/1 compiler bypass control construct
		{logtalk::phrase(_, _, _)}.

	test(phrase_3_02, error(instantiation_error)) :-
		{logtalk << phrase(_, _, _)}.

	test(phrase_3_03, error(instantiation_error)) :-
		{logtalk << phrase([_|_], _)}.

	test(phrase_3_04, error(instantiation_error)) :-
		{logtalk << phrase({_}, _)}.

	test(phrase_3_05, error(type_error(callable,1))) :-
		{logtalk << phrase(1, _, _)}.

	test(phrase_3_06, error(type_error(callable,1))) :-
		{logtalk << phrase({1}, _, _)}.

	test(phrase_3_07, error(type_error(callable,1))) :-
		{logtalk << phrase(([_],{1}), [], [])}.

	test(phrase_3_08, error(type_error(callable,1))) :-
		{logtalk << phrase(([_];{1}), [_], [])}.

	test(phrase_3_09, error(type_error(callable,1))) :-
		{logtalk << phrase(([_];{1}), [_], [])}.

	test(phrase_3_10, error(type_error(list,1))) :-
		{logtalk << phrase(foo, 1, _)}.

	test(phrase_3_11, error(type_error(list,1))) :-
		{logtalk << phrase(foo, _, 1)}.

	test(phrase_3_12, error(type_error(list,[1|2]))) :-
		{logtalk << phrase(foo, [1|2], _)}.

	test(phrase_3_13, error(type_error(list,[1|2]))) :-
		{logtalk << phrase(foo, _, [1|2])}.

	test(phrase_3_14, error(existence_error(procedure,foo/2))) :-
		{logtalk << phrase(foo, _, _)}.

	test(phrase_3_15, true(Input == Rest)) :-
		{logtalk << phrase({}, Input, Rest)}.

	test(phrase_3_16, true(Input == Rest)) :-
		{logtalk << phrase({true}, Input, Rest)}.

	test(phrase_3_17, true(Input == Rest)) :-
		{logtalk << phrase(!, Input, Rest)}.

	test(phrase_3_18, false) :-
		{logtalk << phrase({!, fail}, _, _)}.

	test(phrase_3_19, false) :-
		{logtalk << phrase(({!, fail}; {true}), _, _)}.

	test(phrase_3_20, true(Input == [a,b])) :-
		{logtalk << phrase([a,b], Input, [])}.

	test(phrase_3_21, true(Input == [a,b,b])) :-
		{logtalk << phrase([a,b], Input, [b])}.

:- end_object.
