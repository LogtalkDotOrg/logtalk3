%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(format).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2022-01-27,
		comment is 'Formatted output predicates.'
	]).

	:- public(format/3).
	:- mode(format(@stream_or_alias, +atom, @list), zero_or_one).
	:- mode(format(@stream_or_alias, +list(character_code), @list), zero_or_one).
	:- mode(format(@stream_or_alias, +list(character), @list), zero_or_one).
	:- info(format/3, [
		comment is 'Writes a list of arguments after a format specification to the specified output stream.',
		argnames is ['Stream', 'Format', 'Arguments']
	]).

	:- public(format/2).
	:- mode(format(+atom, @list), zero_or_one).
	:- mode(format(+list(character_code), @list), zero_or_one).
	:- mode(format(+list(character), @list), zero_or_one).
	:- info(format/2, [
		comment is 'Writes a list of arguments after a format specification to the current output stream.',
		argnames is ['Format', 'Arguments']
	]).

	:- if(current_logtalk_flag(prolog_dialect, ciao)).

		format(Stream, Format, Arguments) :-
			{format:format(Stream, Format, Arguments)}.

		format(Format, Arguments) :-
			{format:format(Format, Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		format(Stream, Format, Arguments) :-
			{format:format(Stream, Format, Arguments)}.

		format(Format, Arguments) :-
			{format:format(Format, Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, scryer)).

		format(Stream, Format, Arguments) :-
			(	atom(Format) ->
				atom_chars(Format, Chars),
				{format:format(Stream,  Chars, Arguments)}
			;	{format:format(Stream, Format, Arguments)}
			).

		format(Format, Arguments) :-
			(	atom(Format) ->
				atom_chars(Format, Chars),
				{format:format(Chars,  Arguments)}
			;	{format:format(Format, Arguments)}
			).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		format(Stream, Format, Arguments) :-
			{format:format(Stream, Format, Arguments)}.

		format(Format, Arguments) :-
			{format:format(Format, Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, tau)).

		format(Stream, Format, Arguments) :-
			(	atom(Format) ->
				atom_chars(Format, Chars),
				{format:format(Stream,  Chars, Arguments)}
			;	{format:format(Stream, Format, Arguments)}
			).

		format(Format, Arguments) :-
			(	atom(Format) ->
				atom_chars(Format, Chars),
				{format:format(Chars,  Arguments)}
			;	{format:format(Format, Arguments)}
			).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		format(Stream, Format, Arguments) :-
			{format:format(Stream, Format, Arguments)}.

		format(Format, Arguments) :-
			{format:format(Format, Arguments)}.

	:- else.

		format(Stream, Format, Arguments) :-
			{format(Stream, Format, Arguments)}.

		format(Format, Arguments) :-
			{format(Format, Arguments)}.

	:- endif.

	% linter warning definitions

	:- multifile(user::logtalk_linter_hook/7).
	user::logtalk_linter_hook(
		format::format(Stream, Format, Arguments), suspicious_calls,
		File, Lines, Type, Entity,
		suspicious_call(File, Lines, Type, Entity, format::format(Stream, Format, Arguments), reason(Reason))
	) :-
		verify_arguments(Format, Arguments, Reason).
	user::logtalk_linter_hook(
		format::format(Format, Arguments), suspicious_calls,
		File, Lines, Type, Entity,
		suspicious_call(File, Lines, Type, Entity, format::format(Format, Arguments), reason(Reason))
	) :-
		verify_arguments(Format, Arguments, Reason).

	verify_arguments(_, Arguments, as('arguments is not a list')) :-
		Arguments \== [],
		Arguments \= [_| _],
		!.
	verify_arguments(Format, Arguments, Reason) :-
		nonvar(Format),
		(	atom(Format) ->
			atom_chars(Format, Chars),
			verify_arguments_chars(Chars, Arguments, Reason)
		;	Format = [Head| Tail], atom(Head) ->
			Chars = [Head| Tail],
			verify_arguments_chars(Chars, Arguments, Reason)
		;	Format = [Head| Tail], integer(Head),
			codes_to_chars([Head| Tail], Chars),
			verify_arguments_chars(Chars, Arguments, Reason)
		).

	verify_arguments_chars([], [_| _], due_to('too many arguments')) :-
		!.
	verify_arguments_chars([Char| Chars], [], due_to('too few arguments')) :-
		phrase(control_sequence, [Char| Chars], _),
		!.
	verify_arguments_chars(['~'| Chars], Arguments, Reason) :-
		phrase(non_arg_control_sequence, Chars, Rest),
		!,
		verify_arguments_chars(Rest, Arguments, Reason).
	verify_arguments_chars(['~'| Chars], [_| Arguments], Reason) :-
		!,
		phrase(control_sequence, Chars, Rest),
		verify_arguments_chars(Rest, Arguments, Reason).
	verify_arguments_chars([_ | Chars], Arguments, Reason) :-
		verify_arguments_chars(Chars, Arguments, Reason).

	non_arg_control_sequence -->
		['~'].
	non_arg_control_sequence -->
		['N'].
	non_arg_control_sequence -->
		possible_integer, non_arg_control_code.

	non_arg_control_code -->
		['n'].
	non_arg_control_code -->
		['|'].
	non_arg_control_code -->
		['+'].
	non_arg_control_code -->
		['t'].

	control_sequence -->
		possible_integer, char.

	possible_integer -->
		[Digit], {'0' @=< Digit, Digit @=< '9'}, !, possible_integer.
	possible_integer -->
		[].

	char -->
		[Char], {once(('a' @=< Char, Char @=< 'z'; 'A' @=< Char, Char @=< 'Z'))}.

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.
