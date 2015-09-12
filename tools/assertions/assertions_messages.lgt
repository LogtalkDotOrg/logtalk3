%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/10/01,
		comment is 'Assertions framework default message translations.'
	]).

	:- set_logtalk_flag(debug, off).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	% Quintus Prolog based prefixes (also used in e.g. SICStus Prolog):
	logtalk::message_prefix_stream(information, assertions, '% ', user_output).
	logtalk::message_prefix_stream(warning, assertions, '*     ', user_output).
	logtalk::message_prefix_stream(error, assertions,   '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	% assertion/1 messages
	logtalk::message_tokens(assertion_success(Goal), assertions) -->
		['assertion goal success: ~w'-[Goal], nl].
	logtalk::message_tokens(assertion_failure(Goal), assertions) -->
		['assertion goal failure: ~w'-[Goal], nl].
	logtalk::message_tokens(assertion_error(Goal, Error), assertions) -->
		['assertion goal error: ~w - ~w'-[Goal, Error], nl].

	% assertion/2 messages
	logtalk::message_tokens(assertion_success(Context, Goal), assertions) -->
		['assertion goal success: ~w in context '-[Goal, Context], nl].
	logtalk::message_tokens(assertion_failure(Context, Goal), assertions) -->
		['assertion goal failure: ~w in context ~w'-[Goal, Context], nl].
	logtalk::message_tokens(assertion_error(Context, Goal, Error), assertions) -->
		['assertion goal error: ~w - ~w in context ~w'-[Goal, Error, Context], nl].

:- end_category.
