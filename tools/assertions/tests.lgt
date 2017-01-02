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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/07/07,
		comment is 'Unit tests for the "assertions" tool.'
	]).

	:- uses(assertions, [
		assertion/1,
		assertion/2
	]).

	cover(assertions).

	% assertion/1 tests

	test(assertions_1) :-
		assertion(ground(x)),
		2 is 1 + 1.

	test(assertions_2) :-
		assertion(22 is 2 + 2),
		2 is 1 + 1.

	test(assertions_3) :-
		assertion(1),
		2 is 1 + 1.

	% assertion/2 tests

	test(assertions_4) :-
		assertion(assertions_4, ground(x)),
		2 is 1 + 1.

	test(assertions_5) :-
		assertion(assertions_5, 22 is 2 + 2),
		2 is 1 + 1.

	test(assertions_6) :-
		assertion(assertions_6, 1),
		2 is 1 + 1.

	% goal_expansion/2 tests

	test(assertions_7) :-
		source::p_1.

	test(assertions_8) :-
		source::p_2.

	test(assertions_9) :-
		source::p_3.

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, Kind, assertions, _) :-
		!,
		ground(Message),
		message(Message, Kind).

	message(assertion_success(ground(x)), silent).
	message(assertion_failure(22 is 2+2), error).
	message(assertion_error(1,error(type_error(callable,1),logtalk(call(1),tests))), error).

	message(assertion_success(assertions_4,ground(x)), silent).
	message(assertion_failure(assertions_5,22 is 2+2), error).
	message(assertion_error(assertions_6,1,error(type_error(callable,1),logtalk(call(1),tests))), error).

	message(assertion_success(Context,ground(x)), silent) :-
		check_context(Context).
	message(assertion_failure(Context,22 is 2+2), error) :-
		check_context(Context).
	message(assertion_error(Context,1,error(type_error(callable,1),logtalk(call(1),source))), error) :-
		check_context(Context).

	check_context(file_lines(File,BeginLine-EndLine)) :-
		sub_atom(File, _, _, 0, 'source.lgt'),
		integer(BeginLine), integer(EndLine).

:- end_object.
