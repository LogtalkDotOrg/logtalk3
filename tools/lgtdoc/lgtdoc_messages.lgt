%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


% define a flag to allow the logtalk_tester script to pass the
% option to suppress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- category(lgtdoc_messages).

	:- info([
		version is 4:0:1,
		author is 'Paulo Moura',
		date is 2024-12-02,
		comment is 'Logtalk documentation tool default message translations.'
	]).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, lgtdoc, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, lgtdoc) -->
		message_tokens(Message).

	% linter messages

	message_tokens(missing_entity_directive(Directive, Type, Entity, File, Line)) -->
		['Missing directive: ~q'-[Directive], nl],
		message_context(File, Line, Type, Entity).

	message_tokens(missing_predicate_directive(Directive, Indicator, Type, Entity, File, Line)) -->
		(	{Indicator = _//_} ->
			['Missing ~q directive for non-terminal: ~q'-[Directive, Indicator], nl]
		;	['Missing ~q directive for predicate: ~q'-[Directive, Indicator], nl]
		),
		message_context(File, Line, Type, Entity).

	message_tokens(missing_entity_info_key(Key, Type, Entity, File, Line)) -->
		['Missing info/1 key: ~q'-[Key], nl],
		message_context(File, Line, Type, Entity).

	message_tokens(missing_predicate_info_key(Indicator, Key, Type, Entity, File, Line)) -->
		(	{Indicator = _//_} ->
			['Missing key for non-terminal ~q: ~q'-[Indicator, Key], nl]
		;	['Missing key for predicate ~q: ~q'-[Indicator, Key], nl]
		),
		message_context(File, Line, Type, Entity).

	message_tokens(missing_punctuation(Text, Type, Entity, File, Line)) -->
		['Missing punctuation at the end of text: ~q'-[Text], nl],
		message_context(File, Line, Type, Entity).

	message_tokens(non_standard_exception(Indicator, Exception, Type, Entity, File, Line)) -->
		(	{Indicator = _//_} ->
			['Non-standard exception for non-terminal ~q: ~q'-[Indicator, Exception], nl]
		;	['Non-standard exception for predicate ~q: ~q'-[Indicator, Exception], nl]
		),
		message_context(File, Line, Type, Entity).

	message_tokens(invalid_date(Date, Type, Entity, File, Line)) -->
		['Invalid date in info/1 directive: ~q'-[Date], nl],
		message_context(File, Line, Type, Entity).

	message_tokens(date_in_the_future(Date, Type, Entity, File, Line)) -->
		['Date in info/1 directive is in the future: ~q'-[Date], nl],
		message_context(File, Line, Type, Entity).

	% auxiliary non-terminals and predicates

	message_context(Path, Line, Type, Entity) -->
		{suppress_path_prefix(Path, ShortPath)},
		['  while compiling ~w ~q'-[Type, Entity], nl],
		(	{Line == 0} ->
			['  in auxiliary clause generated for file ~w'-[ShortPath], nl, nl]
		;	{Line == 1} ->
			['  in file ~w at line 1'-[ShortPath], nl, nl]
		;	{Line == -1} ->
			['  in file ~w'-[ShortPath], nl, nl]
		;	['  in file ~w below line ~d'-[ShortPath, Line], nl, nl]
		).

	suppress_path_prefix(Path, ShortPath) :-
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

:- end_category.
