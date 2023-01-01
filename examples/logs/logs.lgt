%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- category(logging).

	:- public([
		init_log/0,
		add_log_entry/1,
		print_log/0,
		% public access to log entries
		log_entry/2
	]).

	% table of log entries
	:- private(log_/2).
	:- dynamic(log_/2).

	% used to represent event time in the log
	:- op(600, xfy, (:)).

	% retractall/1 retracts clauses in "this", i.e. in the object importing the category
	init_log :-
		retractall(log_(_, _)),
		add_log_entry(start).

	% assertz/1 asserts clauses in "this", i.e. in the object importing the category
	add_log_entry(Entry) :-
		date::today(Year, Month, Day),
		time::now(Hours, Mins, Secs),
		assertz(log_(Year/Month/Day-Hours:Mins:Secs, Entry)).

	% log_/2 is a private dynamic predicate; calls occur in "this",
	% i.e. in the context of the object importing the category
	print_log :-
		log_(Date, Entry),
		write(Date), write(' - '), write(Entry), nl,
		fail.
	print_log.

	log_entry(Date, Entry) :-
		log_(Date, Entry).

:- end_category.


:- object(object,
	imports(logging)).

	% the following two initialization goals are equivalent because, in the case
	% of the initialization/1 directive, "self" and "this" are the same object:

	% starts lookup for init_log/0 in "self"
	%:- initialization(::init_log).

	% starts lookup for init_log/0 in "this"
	:- initialization(^^init_log).

:- end_object.
