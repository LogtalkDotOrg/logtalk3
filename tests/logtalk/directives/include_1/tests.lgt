%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- include('file.pl').


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2018-07-03,
		comment is 'Unit tests for the include/1 built-in directive.'
	]).

	:- uses(os, [decompose_file_name/4]).
	:- uses(list, [memberchk/2]).

	:- include('file.pl').

	test(include_1_01) :-
		findall(X, {a(X)}, Xs),
		Xs == [1, 2, 3].

	test(include_1_02) :-
		findall(X, {b(_, X)}, Xs),
		Xs == [1, 2, 3].

	test(include_1_03) :-
		\+ {current_predicate(c/3)}.

	test(include_1_04) :-
		findall(X, a(X), Xs),
		Xs == [1, 2, 3].

	test(include_1_05) :-
		findall(X, b(_, X), Xs),
		Xs == [1, 2, 3].

	test(include_1_06) :-
		findall(X-Y, foo(X,Y), Pairs),
		Pairs == [a-b, b-c, c-d].

	test(include_1_07) :-
		\+ current_predicate(c/3).

	% test multiple initialization/1 directives
	% in main file and in the included files

	test(include_1_08) :-
		main<<a,
		main<<i(main_1).

	test(include_1_09) :-
		main<<a,
		main<<i(main_2).

	test(include_1_10) :-
		main<<b,
		main<<i(include_1_1).

	test(include_1_11) :-
		main<<b,
		main<<i(include_1_2).

	test(include_1_12) :-
		main<<c,
		main<<i(include_2).

	% test multiple initialization/1 directives order

	test(include_1_13) :-
		findall(Arg, main<<i(Arg), Args),
		Args == [main_1, include_1_1, include_2, include_1_2, main_2].

	% test reflection API

	test(include_1_14) :-
		object_property(main, declares(b/0, Properties)),
		memberchk(include(Include1), Properties),
		object_property(main, file(_,Directory)),
		decompose_file_name(Include1, Directory, include_1, '.pl').

	test(include_1_15) :-
		object_property(main, declares(b/0, Properties)),
		memberchk(line_count(Line), Properties),
		integer(Line).

	test(include_1_16) :-
		object_property(main, defines(b/0, Properties)),
		memberchk(include(Include1), Properties),
		object_property(main, file(_,Directory)),
		decompose_file_name(Include1, Directory, include_1, '.pl').

	test(include_1_17) :-
		object_property(main, defines(b/0, Properties)),
		memberchk(line_count(Line), Properties),
		integer(Line).

	test(include_1_18) :-
		object_property(main, declares(c/0, Properties)),
		memberchk(include(Include2), Properties),
		object_property(main, file(_,Directory)),
		decompose_file_name(Include2, Directory, include_2, '.pl').

	test(include_1_19) :-
		object_property(main, declares(c/0, Properties)),
		memberchk(line_count(Line), Properties),
		integer(Line).

	test(include_1_20) :-
		object_property(main, defines(c/0, Properties)),
		memberchk(include(Include2), Properties),
		object_property(main, file(_,Directory)),
		decompose_file_name(Include2, Directory, include_2, '.pl').

	test(include_1_21) :-
		object_property(main, defines(c/0, Properties)),
		memberchk(line_count(Line), Properties),
		integer(Line).

	test(include_1_22) :-
		{plain_1(X)},
		X == 1.

	test(include_1_23) :-
		{up::plain_1(X)},
		X == 1.

	test(include_1_24) :-
		{plain_2(X)},
		X == 1.

	test(include_1_25) :-
		{sub::plain_2(X)},
		X == 1.

:- end_object.
