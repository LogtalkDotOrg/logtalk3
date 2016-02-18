%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.22,
		author is 'Parker Jones and Paulo Moura',
		date is 2013/08/10,
		comment is 'Unit tests for the "tabling" example.'
	]).

	cover(fibonacci).
	cover(paths).

	:- if(current_logtalk_flag(prolog_dialect, yap)).
	cover(mdt_paths_first).
	cover(mdt_paths_min).
	cover(mdt_paths_min_all).
	:- endif.

	test(tabling_1) :-
		setof(Y, paths::path(1, Y), Ys),
		Ys = [2, 3, 4, 5].

	test(tabling_2) :-
		fibonacci::fib(30, F),
		F == 1346269.

	:- if(current_logtalk_flag(prolog_dialect, yap)).

	test(tabling_3) :-
		setof((Z, N), mdt_paths_first::path(a, Z, N), L),
		L == [(a, 2), (b, 1)].

	test(tabling_4) :-
		setof((Z, C), mdt_paths_min::path(a, Z, C), L),
		L == [(b, 1), (c, 2), (d, 3)].

	test(tabling_5) :-
		setof((Z, C, N), mdt_paths_min_all::path(a, Z, C, N), L),
		L == [(b, 2, 1), (b, 2, 2), (c, 1, 1)].

	:- endif.

:- end_object.
