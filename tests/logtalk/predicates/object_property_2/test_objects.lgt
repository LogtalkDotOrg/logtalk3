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


:- set_logtalk_flag(source_data, on).


:- object(test_object).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2016/10/10,
		comment is 'Sample object for testing with the `source_data` flag turned on.']).

	:- set_logtalk_flag(complements, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(context_switching_calls, deny).
	:- set_logtalk_flag(events, allow).

	:- public(a/1).
	:- if(current_logtalk_flag(coinduction, supported)).
		:- coinductive(a/1).
	:- endif.
	a(1).

	:- protected(b/2).
	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized(b/2).
	:- endif.
	b(1, 2).
	b(2, 1).

	:- private(c/3).
	:- dynamic(c/3).
	c(1, 2, 3).
	c(2, 3, 1).
	c(3, 1, 2).

	d(1, 2, 3, 4).
	d(2, 3, 4, 1).
	d(3, 4, 1, 2).
	d(4, 1, 2, 3).

	:- private(e/5).

	caller1 :-
		local.

	local.

	caller2 :-
		logtalk::expand_library_path(home, _).

	caller3 :-
		phrase(logtalk::message_tokens(_,_), _, _).

	:- uses(logtalk, [
		loaded_file/1 as loaded/1, message_tokens//2 as tokens//2
	]).

	caller4 :-
		loaded(_).

	caller5 :-
		phrase(tokens(_,_), _, _).

:- end_object.


:- object(empty_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/02/19,
		comment is 'Empty object for testing validity of object properties.'
	]).

:- end_object.


:- object(built_in_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Built-in object for testing determinism of object properties.']).

	:- built_in.

:- end_object.


:- object(dynamic_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Dynamic object for testing determinism of object properties.']).

	:- (dynamic).

:- end_object.


:- object(debug_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Object compiled in debug mode for testing determinism of object properties.']).

	:- set_logtalk_flag(debug, on).

:- end_object.


:- object(options_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Object compiled with optional features turned on for testing determinism of object properties.']).

	:- set_logtalk_flag(events, allow).
	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(complements, allow).

:- end_object.


:- object(threaded_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Object compiled with threaded calls support for testing determinism of object properties.']).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

:- end_object.
