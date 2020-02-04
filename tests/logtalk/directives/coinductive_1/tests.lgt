%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- set_logtalk_flag(undefined_predicates, silent).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2019-03-29,
		comment is 'Unit tests for the coinductive/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(a/1).
	:- coinductive(a/1).

	test(coinductive_1_01) :-
		predicate_property(a(_), coinductive(Template)),
		Template == a(+).


	:- public(b//0).
	:- coinductive(b//0).

	test(coinductive_1_02) :-
		predicate_property(b(_,_), coinductive(Template)),
		Template == b(+,+).

	:- public(c/2).
	:- coinductive(c(+, -)).

	test(coinductive_1_03) :-
		predicate_property(c(_, _), coinductive(Template)),
		Template == c(+, -).

	% calls to predicates declared coinductive but not defined
	% must fail instead of throwing an existence error

	:- coinductive(d/3).

	test(coinductive_1_04) :-
		\+ d(_, _, _).

:- end_object.
