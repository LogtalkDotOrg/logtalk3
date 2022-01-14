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
:- initialization((
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
	create_logtalk_flag(test_unit_name, '', [type(atom), keep(true)]),
	create_logtalk_flag(issue_server, '', [type(atom), keep(true)])
)).


:- object(issue_creator).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2022-01-14,
		comment is 'Support for automatically creating bug report issues for failed tests in GitHub or GitLab servers.'
	]).

	:- uses(git, [
		commit_hash_abbreviated/2
	]).

	:- uses(os, [
		decompose_file_name/3, shell/1
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message),
		% allow default processing of the messages
		fail.

	% failed tests
	message_hook(failed_test(Object, Test, File, Position, Reason, Note, Time)) :-
		command(Object, Test, File, Position, Reason, Note, Time, Command),
		shell(Command).
	message_hook(non_deterministic_success(Object, Test, File, Position, Note, Time)) :-
		command(Object, Test, File, Position, non_deterministic_success, Note, Time, Command),
		shell(Command).

	command(Object, Test, File, Position, Reason, Note, Time, Command) :-
		decompose_file_name(File, Directory, _),
		commit_hash_abbreviated(Directory, Hash),
		% bypass the compiler as the flags are only created after loading this file
		{current_logtalk_flag(test_unit_name, TestSet)},
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, Suffix, File) ->
			ShortFile = Suffix
		;	ShortFile = File
		),
		title(Test, TestSet, Title),
		escape_double_quotes(Title, EscapedTitle),
		description(Object, ShortFile, Position, Reason, Note, Time, Hash, Description),
		escape_double_quotes(Description, EscapedDescription),
		issue_server(Server),
		command(Server, EscapedTitle, EscapedDescription, Command).

	command(github, Title, Description, Command) :-
		atomic_list_concat(['gh issue create --title "', Title, '" --body "', Description, '"'], Command).
	command(gitlab, Title, Description, Command) :-
		atomic_list_concat(['glab issue create --title "', Title, '" --description "', Description, '"'], Command).

	title(Test, TestSet, Title) :-
		to_atom(Test, TestAtom),
		atomic_list_concat(['Test ', TestAtom, ' of test set ', TestSet, ' failed: '], Title).

	description(Object, File, Position, Reason, Note, Time, Hash, Description) :-
		to_atom(Object, ObjectAtom),
		to_atom(Position, PositionAtom),
		to_atom(Reason, ReasonAtom),
		to_atom(Note, NoteAtom),
		to_atom(Time, TimeAtom),
		atomic_list_concat([
			'Test object: ', ObjectAtom, '\n',
			'Test file:   ', File, ':', PositionAtom, '\n', '\n',
			'Failure:     ', ReasonAtom, '\n', '\n',
			'Note:        ', NoteAtom,  '\n', '\n',
			'Time:        ', TimeAtom, '\n',
			'Commit hash: ', Hash, '\n'
		], Description).

	issue_server(Server) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(issue_server, Server0)},
		server(Server0, Server).

	server('',     github).  % default
	server(github, github).
	server(gitlab, gitlab).

	to_atom(Term, Atom) :-
		(	atom(Term) ->
			Atom = Term
		;	write_to_atom(Term, Atom)
		).

	escape_double_quotes(Atom, EscapedAtom) :-
		atom_chars(Atom, Chars),
		escape_double_quotes_in_chars(Chars, EscapedChars),
		atom_chars(EscapedAtom, EscapedChars).

	escape_double_quotes_in_chars([], []).
	escape_double_quotes_in_chars(['"'| Chars], ['\\', '"'| EscapedChars]) :-
		!,
		escape_double_quotes_in_chars(Chars, EscapedChars).
	escape_double_quotes_in_chars([Char| Chars], [Char| EscapedChars]) :-
		escape_double_quotes_in_chars(Chars, EscapedChars).

:- end_object.
