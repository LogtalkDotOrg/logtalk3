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


% define a flag to allow the logtalk_tester script to pass the
% option to suppress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- category(lgtdoc_messages).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2022-10-22,
		comment is 'Logtalk documentation tool default message translations.'
	]).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, lgtdoc) -->
		message_tokens(Message).

	% linter messages

	message_tokens(missing_entity_directive(Directive, Type, Entity, File, Line)) -->
		['Missing ~q directive for ~w: ~q'-[Directive, Type, Entity], nl],
		file_context(File, Line).

	message_tokens(missing_predicate_directive(Directive, Entity, Indicator, File, Line)) -->
		(	{Indicator = _//_} ->
			['Missing ~q directive for ~q non-terminal: ~q'-[Directive, Entity, Indicator], nl]
		;	['Missing ~q directive for ~q predicate: ~q'-[Directive, Entity, Indicator], nl]
		),
		file_context(File, Line).

	message_tokens(missing_info_key(Entity, Key, File, Line)) -->
		['Missing key for ~q: ~q'-[Entity, Key], nl],
		file_context(File, Line).

	message_tokens(missing_info_key(Entity, Indicator, Key, File, Line)) -->
		(	{Indicator = _//_} ->
			['Missing key for ~q non-terminal ~q: ~q'-[Entity, Indicator, Key], nl]
		;	['Missing key for ~q predicate ~q: ~q'-[Entity, Indicator, Key], nl]
		),
		file_context(File, Line).

	message_tokens(missing_period(Entity, Text, File, Line)) -->
		['Missing period at the end of text for ~q: ~q'-[Entity, Text], nl],
		file_context(File, Line).

	message_tokens(non_standard_exception(Entity, Indicator, Exception, File, Line)) -->
		(	{Indicator = _//_} ->
			['Non-standard exception for ~q non-terminal ~q: ~q'-[Entity, Indicator, Exception], nl]
		;	['Non-standard exception for ~q predicate ~q: ~q'-[Entity, Indicator, Exception], nl]
		),
		file_context(File, Line).

	message_tokens(invalid_date(Entity, Date, File, Line)) -->
		['Invalid date in info/1 directive for ~q: ~q'-[Entity, Date], nl],
		file_context(File, Line).

	% auxiliary non-terminals and predicates

	file_context(Path, -1) -->
		!,
		{suppress_path_prefix(Path, ShortPath)},
		['  in file ~w'-[ShortPath], nl, nl].
	file_context(Path, Line) -->
		{suppress_path_prefix(Path, ShortPath)},
		['  in file ~w below line ~d'-[ShortPath, Line], nl, nl].

	suppress_path_prefix(Path, ShortPath) :-
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

:- end_category.
