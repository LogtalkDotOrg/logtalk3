%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- include(logtalk_user('tests/logtalk/directives/include_1/file.pl')).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/10/16,
		comment is 'Unit tests for the include/1 built-in directive.'
	]).

	:- include(logtalk_user('tests/logtalk/directives/include_1/file.pl')).

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
		\+ current_predicate(c/3).

	% test multiple initialization/1 directives
	% in main file and in the included files

	test(include_1_07) :-
		main<<a,
		main<<main.

	test(include_1_08) :-
		main<<b,
		main<<include_1.

	test(include_1_09) :-
		main<<c,
		main<<include_2.

:- end_object.
