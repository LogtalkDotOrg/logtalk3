%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- category(test_category).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2016/11/02,
		comment is 'Sample category for testing with the `source_data` flag turned on.'
	]).

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

	updater1 :-
		asserta(c(1,1,1)).

	updater2 :-
		assertz(c(1,1,1)).

	updater3 :-
		retract(c(1,2,3)).

	updater4 :-
		retractall(c(_,_,_)).

	updater5 :-
		clause(c(_,_,_), true).

	updater6 :-
		abolish(c/3).

	updater1s :-
		::asserta(c(1,1,1)).

	updater2s :-
		::assertz(c(1,1,1)).

	updater3s :-
		::retract(c(1,2,3)).

	updater4s :-
		::retractall(c(_,_,_)).

	updater5s :-
		::clause(c(_,_,_), true).

	updater6s :-
		::abolish(c/3).

	updater1o :-
		logtalk::asserta(c(1,1,1)).

	updater2o :-
		logtalk::assertz(c(1,1,1)).

	updater3o :-
		logtalk::retract(c(1,2,3)).

	updater4o :-
		logtalk::retractall(c(_,_,_)).

	updater5o :-
		logtalk::clause(c(_,_,_), true).

	updater6o :-
		logtalk::abolish(c/3).

:- end_category.


:- category(empty_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/02/19,
		comment is 'Empty category for testing validity of category properties.'
	]).

:- end_category.


:- category(built_in_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Built-in category for testing determinism of category properties.'
	]).

	:- built_in.

:- end_category.


:- category(dynamic_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Dynamic category for testing determinism of category properties.'
	]).

	:- (dynamic).

:- end_category.


:- category(debug_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Category compiled in debug mode for testing determinism of category properties.'
	]).

	:- set_logtalk_flag(debug, on).

:- end_category.


:- category(events_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Category compiled with event support for testing determinism of category properties.'
	]).

	:- set_logtalk_flag(events, allow).

:- end_category.
