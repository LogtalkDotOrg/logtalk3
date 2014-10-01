%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
