%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(user,
	implements((expanding, forwarding, monitoring))).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2024-11-11,
		comment is 'Pseudo-object representing the plain Prolog database. Can be used as a monitor by defining ``before/3`` and ``after/3`` predicates. Can be used as a hook object by defining ``term_expansion/2`` and ``goal_expansion/2`` multifile and dynamic predicates.'
	]).

	:- built_in.

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(complements, deny).
	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- elif(current_logtalk_flag(engines, supported)).
		:- threaded.
	:- endif.

	% this forward/1 handler definition illustrates how messages to the
	% "user" pseudo-object could be translated to plain Prolog calls but
	% it's not necessary or used as the Logtalk compiler already performs
	% this translation
	:- meta_predicate(forward(0)).
	forward(Message) :-
		{Message}.

	% allow the "user" pseudo-object to be used as an event monitor;
	% requires before/3 and after/3 predicates to be defined in "user"

	before(Object, Message, Sender) :-
		user::before(Object, Message, Sender).

	after(Object, Message, Sender) :-
		user::after(Object, Message, Sender).

	% ensure attempts to use this object as monitor without definitions
	% for the before/3 and after/3 predicates in "user" will not result
	% in predicate existence errors

	:- multifile(before/3).
	:- multifile(after/3).

	% ensure that setting the "hook" flag to "user" will not result in
	% predicate existence errors during compilation of source files as
	% the expansion predicates are only declared in some of the supported
	% backend Prolog compilers

	:- multifile(term_expansion/2).
	:- dynamic(term_expansion/2).

	:- multifile(goal_expansion/2).
	:- dynamic(goal_expansion/2).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with predicates
	% that are called from a file but not defined in that file
	:- multifile(before/3).
	:- multifile(after/3).
:- endif.
