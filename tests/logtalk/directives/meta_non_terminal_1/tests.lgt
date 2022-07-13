%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2022-07-13,
		comment is 'Unit tests for the meta_non_terminal/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(foo//1).
	:- meta_non_terminal(foo(0)).

	test(meta_non_terminal_1_01, true(Template == foo(2, *, *))) :-
		predicate_property(foo(_,_,_), non_terminal(foo//1)),
		predicate_property(foo(_,_,_), meta_predicate(Template)).

	% existentially-qualified closures are currently not supported
	% :- public(bar/2).
	% :- meta_predicate(bar(^, *)).

	:- public(baz//3).
	:- meta_non_terminal(baz(2, *, *)).

	test(meta_non_terminal_1_02, true(Template == baz(4, *, *, *, *))) :-
		predicate_property(baz(_,_,_,_,_), non_terminal(baz//3)),
		predicate_property(baz(_,_,_,_,_), meta_predicate(Template)).

	:- public(qux//2).
	:- meta_non_terminal(qux(::, *)).

	test(meta_non_terminal_1_03, true(Template == qux(::, *, *, *))) :-
		predicate_property(qux(_,_,_,_), non_terminal(qux//2)),
		predicate_property(qux(_,_,_,_), meta_predicate(Template)).

	% test the use of pre-compiled closures passed as argument to
	% the phrase/2-3 predicates when compiling in optimal mode
	test(meta_non_terminal_1_04, true((Lower == one, Upper == 'ONE'))) :-
		test_object_2::p(1, Lower, Upper).

:- end_object.
