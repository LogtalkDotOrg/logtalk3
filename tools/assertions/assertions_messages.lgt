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


:- category(assertions_messages).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2014/10/22,
		comment is 'Assertions framework default message translations.'
	]).

	:- set_logtalk_flag(debug, off).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, assertions, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	% Quintus Prolog based prefixes (also used in e.g. SICStus Prolog):
	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, assertions) -->
		message_tokens(Message).

	% assertion/1 messages
	message_tokens(assertion_success(Goal)) -->
		['assertion goal success: ~w'-[Goal], nl].
	message_tokens(assertion_failure(Goal)) -->
		['assertion goal failure: ~w'-[Goal], nl].
	message_tokens(assertion_error(Goal, Error)) -->
		['assertion goal error: ~w - ~w'-[Goal, Error], nl].

	% assertion/2 messages
	message_tokens(assertion_success(Context, Goal)) -->
		['assertion goal success: ~w in context '-[Goal, Context], nl].
	message_tokens(assertion_failure(Context, Goal)) -->
		['assertion goal failure: ~w in context ~w'-[Goal, Context], nl].
	message_tokens(assertion_error(Context, Goal, Error)) -->
		['assertion goal error: ~w - ~w in context ~w'-[Goal, Error, Context], nl].

:- end_category.
