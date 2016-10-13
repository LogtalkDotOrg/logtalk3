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


:- object(tests_dialects,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/10/13,
		comment is 'Unit tests for the "lgtunit" tool testing dialects.'
	]).

	% test/1 dialect

	test(test_1_01) :-
		true.

	test(test_1_02) :-
		\+ fail.

	test(test_1_03) :-
		catch(throw(error), _, true).

	% test/2 dialect

	test(test_2_01, true) :-
		true.

	test(test_2_02, true(integer(N))) :-
		N = 1.

	test(test_2_03, deterministic) :-
		true.

	test(test_2_04, deterministic(integer(N))) :-
		N = 1.

	test(test_2_05, fail) :-
		fail.

	test(test_2_06, error(my_error)) :-
		throw(error(my_error, my_error_context)).

	test(test_2_07, errors([my_error1,my_error2])) :-
		throw(error(my_error2, my_error_context)).

	test(test_2_08, ball(ball)) :-
		throw(ball).

	test(test_2_09, balls([ball1,ball2])) :-
		throw(ball1).

	% test/3 dialect

	test(test_3_01, true, [condition(true), setup(true), cleanup(true), note(test_3_01)]) :-
		true.

	test(test_3_02, true(integer(N)), [condition(true), setup(true), cleanup(true), note(test_3_02)]) :-
		N = 1.

	test(test_3_03, deterministic, [condition(true), setup(true), cleanup(true), note(test_3_03)]) :-
		true.

	test(test_3_04, deterministic(integer(N)), [condition(true), setup(true), cleanup(true), note(test_3_04)]) :-
		N = 1.

	test(test_3_05, fail, [condition(true), setup(true), cleanup(true), note(test_3_05)]) :-
		fail.

	test(test_3_06, error(my_error), [condition(true), setup(true), cleanup(true), note(test_3_06)]) :-
		throw(error(my_error, my_error_context)).

	test(test_3_07, errors([my_error1,my_error2]), [condition(true), setup(true), cleanup(true), note(test_3_07)]) :-
		throw(error(my_error2, my_error_context)).

	test(test_3_08, ball(ball), [condition(true), setup(true), cleanup(true), note(test_3_08)]) :-
		throw(ball).

	test(test_3_09, balls([ball1,ball2]), [condition(true), setup(true), cleanup(true), note(test_3_09)]) :-
		throw(ball1).

	% "explicit" dialect

	succeeds(explicit_01) :-
		true.

	deterministic(explicit_02) :-
		true.

	fails(explicit_03) :-
		fail.

	throws(explicit_04, ball) :-
		throw(ball).

	throws(explicit_05, [ball1,ball2]) :-
		throw(ball1).

	%  dialect

	quick_check(quick_check_01, integer(+integer), []).

	quick_check(quick_check_02, integer(+integer), [n(25)]).

	quick_check(quick_check_03, integer(+integer)).

:- end_object.
