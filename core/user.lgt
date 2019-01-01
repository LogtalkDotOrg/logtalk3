%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
	implements((expanding, forwarding))).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2018/05/24,
		comment is 'Pseudo-object "user" representing the plain Prolog database.'
	]).

	:- built_in.

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(complements, deny).
	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	% this forward/1 handler definition illustrates how messages to the
	% "user" pseudo-object could be translated to plain Prolog calls but
	% it's not necessary or used as the Logtalk compiler already performs
	% this translation
	forward(Message) :-
		{call(Message)}.

	% ensure that setting the "hook" flag to "user" will not result in
	% predicate existence errors during compilation of source files as
	% the expansion predicates are only declared in some of the supported
	% backend Prolog compilers

	:- multifile(user::term_expansion/2).
	:- dynamic(user::term_expansion/2).

	:- multifile(user::goal_expansion/2).
	:- dynamic(user::goal_expansion/2).

:- end_object.
