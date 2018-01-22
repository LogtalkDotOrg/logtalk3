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


:- object(user,
	implements(forwarding)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/11/06,
		comment is 'Pseudo-object "user" representing the plain Prolog database.'
	]).

	:- built_in.

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(complements, deny).
	:- set_logtalk_flag(events, allow).
	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	% this forward/1 handler definition illustrates how messages to the
	% "user" pseudo-object could be translated to plain Prolog calls but
	% it's not necessary or used as the Logtalk compiler already performs
	% this translation
	forward(Message) :-
		{call(Message)}.

:- end_object.
