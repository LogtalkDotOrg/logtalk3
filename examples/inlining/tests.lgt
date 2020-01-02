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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2019/01/05,
		comment is 'Unit tests for the "inlining" example.'
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	% test "inline" predicate property

	test(inlining_01) :-
		inlining::predicate_property(integer(_), inline).

	test(inlining_02) :-
		findall(
			Property,
			inlining::predicate_property(integer(_), Property),
			Properties
		),
		ground(Properties),
		memberchk(inline, Properties).

	test(inlining_03) :-
		inlining::predicate_property(map(_,_), inline).

	test(inlining_04) :-
		findall(
			Property,
			inlining::predicate_property(map(_,_), Property),
			Properties
		),
		ground(Properties),
		memberchk(inline, Properties).

	test(inlining_05) :-
		inlining::predicate_property(a(_,_), inline).

	test(inlining_06) :-
		findall(
			Property,
			inlining::predicate_property(a(_,_), Property),
			Properties
		),
		ground(Properties),
		memberchk(inline, Properties).

	test(inlining_07) :-
		\+ inlining::predicate_property(any(_,_), inline).

	test(inlining_08) :-
		findall(
			Property,
			inlining::predicate_property(any(_,_), Property),
			Properties
		),
		ground(Properties),
		\+ member(inline, Properties).

	% test "inline" predicate definition property

	test(inlining_09) :-
		object_property(inlining, defines(integer/1, Properties)),
		ground(Properties),
		memberchk(inline, Properties).

	test(inlining_10) :-
		object_property(inlining, defines(map/2, Properties)),
		ground(Properties),
		memberchk(inline, Properties).

	test(inlining_11) :-
		object_property(inlining, defines(a/2, Properties)),
		ground(Properties),
		memberchk(inline, Properties).

	test(inlining_12) :-
		object_property(inlining, defines(any/2, Properties)),
		ground(Properties),
		\+ member(inline, Properties).

	:- if(current_logtalk_flag(modules, supported)).

		:- if(current_module(lists)).

			test(inlining_13) :-
				findall(X, inlining::member(X, [1,2,3]), L),
				L == [1,2,3].

			test(inlining_14) :-
				object_property(inlining, defines(member/2, Properties)),
				ground(Properties),
				memberchk(inline, Properties).

		:- endif.

	:- endif.

:- end_object.
