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


:- object(tests(_Index_),
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2020/01/23,
		comment is 'Unit tests for the use_module/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- use_module(module, [
		p/1, q/1 as a1/1, r/1:a2/1, s(1, Atom) as a3(Atom), t(_Index_, Value) as a4(Value)
	]).

	test(use_module_2_01, true(X == 1)) :-
		p(X).

	test(use_module_2_2, true(X == 2)) :-
		a1(X).

	test(use_module_2_3, true(X == 3)) :-
		a2(X).

	test(uses_2_03, true(Xs == [one])) :-
		findall(X, a3(X), Xs).

	test(uses_2_04, true(Xs == [x,y,z])) :-
		findall(X, a4(X), Xs).

:- end_object.
