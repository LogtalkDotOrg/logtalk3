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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/06/30,
		comment is 'Unit tests for the if/1 and other conditional compilation built-in directives.'
	]).

	:- uses(user, [aa/1, bb/1, cc/1, dd/1, ee/1, ff/1, gg/1, hh/1, ii/1, jj/1, ll/1, zz/1]).

	test(if_endif_0) :-
		aa(0).
	test(if_endif_1) :-
		aa(1).
	test(if_endif_2) :-
		\+ aa(2).

	test(if_else_endif_0) :-
		bb(0).
	test(if_else_endif_1) :-
		bb(1), \+ bb(2).
	test(if_else_endif_2) :-
		\+ bb(3), bb(4).

	test(if_else_if_endif_1) :-
		bb(5), \+ bb(6), bb(7), bb(8), \+ bb(9), bb(10).
	test(if_else_if_endif_2) :-
		bb(11), \+ bb(12), bb(13), bb(14), \+ bb(15), bb(16).
	test(if_else_if_endif_3) :-
		bb(17), \+ bb(18), bb(19), \+ bb(20), bb(21), bb(22).
	test(if_else_if_endif_4) :-
		bb(23), \+ bb(24), bb(25), \+ bb(26), \+ bb(27), bb(28).

	test(if_elif_else_endif_0) :-
		cc(0).
	test(if_elif_else_endif_1) :-
		cc(1), \+ cc(2), \+ cc(3).
	test(if_elif_else_endif_2) :-
		cc(4), \+ cc(5), cc(6), \+ cc(7).
	test(if_elif_else_endif_3) :-
		cc(8), \+ cc(9), \+ cc(10), cc(11).
	test(if_elif_else_endif_4) :-
		cc(12), \+ cc(13), \+ cc(14), cc(15), \+ cc(16).

	test(if_elif_elif_else_endif_0) :-
		dd(1), \+ dd(2), \+ dd(3), \+ dd(4).

	test(if_elif_elif_else_endif_1) :-
		\+ ee(5), \+ ee(6), \+ ee(7), ee(8).

	test(if_elif_if_0) :-
		ff(1), \+ ff(2), \+ ff(3), \+ ff(4), \+ ff(5), \+ ff(6), \+ ff(7).

	test(if_if_elif_0) :-
		gg(1), gg(2), \+ gg(3), \+ gg(4), \+ gg(5), \+ gg(6), \+ gg(7).

	test(if_elif_if_1) :-
		\+ hh(1), \+ hh(2), \+ hh(3), \+ hh(4), \+ hh(5), \+ hh(6), hh(7).

	test(if_elif_else_0) :-
		\+ ii(1), ii(2), \+ ii(3), \+ ii(4), \+ ii(5), \+ ii(6), \+ ii(7).

	test(if_if_endif_0) :-
		jj(0).
	test(if_if_endif_1) :-
		jj(1), jj(2),
		jj(3), \+ jj(4), 
		jj(5), jj(6), \+ jj(7),
		jj(8), \+ jj(9), jj(10),
		jj(11),
		\+ jj(12).

	test(if_elif_endif_0) :-
		ll(0).
	test(if_elif_endif_1) :-
		\+ ll(1), \+ ll(2),
		\+ ll(3), \+ ll(4), \+ ll(5),
		\+ ll(6), \+ ll(7), \+ ll(8), \+ ll(9),
		\+ ll(10),
		\+ ll(11),
		ll(12).

	test(if_end_of_file_0) :-
		zz(0).

:- end_object.
