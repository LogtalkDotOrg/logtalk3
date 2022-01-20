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


:- initialization((
	% define a flag to allow the logtalk_tester script to pass the
	% option to supress the test file and directory path prefix
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
	% define a flag to allow the logtalk_tester script to pass the type of
	% server hosting the issue tracker where the bug reports will be created
	create_logtalk_flag(issue_server, '', [type(atom), keep(true)]),
	% define a flag to allow the logtalk_tester script to pass the bug reports label(s)
	create_logtalk_flag(issue_labels, bug, [type(atom), keep(true)]),
	% define a flag to allow the logtalk_tester script to pass the
	% base URL for generating links to test files
	create_logtalk_flag(tests_base_url, '', [type(atom), keep(true)])
)).


:- object(issue_creator).

	:- info([
		version is 0:12:0,
		author is 'Paulo Moura',
		date is 2022-01-20,
		comment is 'Support for automatically creating bug report issues for failed tests in GitHub or GitLab servers.',
		remarks is [
			'Usage' - 'This tool is automatically loaded and used from the ``logtalk_tester`` automation script when using its ``-b`` option.'
		]
	]).

	:- uses(git, [
		branch/2, commit_hash_abbreviated/2, commit_author/2,
		commit_date/2, commit_message/2, commit_log/3
	]).

	:- uses(os, [
		decompose_file_name/3, operating_system_type/1, shell/1
	]).

	:- uses(term_io, [
		write_term_to_atom/3
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
		\+ flaky_test_failure(Note),
		create_bug_report(Object, Test, File, Position, Reason, Note, Time).
	message_hook(non_deterministic_success(Object, Test, File, Position, Note, Time)) :-
		\+ flaky_test_failure(Note),
		create_bug_report(Object, Test, File, Position, non_deterministic_success, Note, Time).

	flaky_test_failure(Note) :-
		atom(Note),
		sub_atom(Note, _, _, _, flaky), !.

	create_bug_report(Object, Test, File, Position, Reason, Note, Time) :-
		decompose_file_name(File, Directory, _),
		branch(Directory, Branch),
		commit_hash_abbreviated(Directory, Hash),
		commit_author(Directory, Author),
		commit_date(Directory, Date),
		commit_message(Directory, Message),
		commit_log(Directory, '%aL', Assignee),
		% bypass the compiler as the flags are only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortFile, File) ->
			true
		;	ShortFile = File
		),
		(	atom_concat(Prefix, ShortDirectory, Directory) ->
			true
		;	ShortDirectory = Directory
		),
		issue_server(Server),
		operating_system_type(OperatingSystem),
		title(Test, ShortDirectory, Title),
		% bypass the compiler as the flags are only created after loading this file
		{current_logtalk_flag(issue_labels, Labels)},
		is_new_issue(Server, OperatingSystem, Title, Labels),
		escape_double_quotes(Title, EscapedTitle),
		description(Object, ShortFile, Position, Reason, Note, Time, Branch, Hash, Author, Date, Message, Description),
		escape_double_quotes(Description, EscapedDescription),
		create_bug_report(Server, OperatingSystem, EscapedTitle, EscapedDescription, Labels, Assignee).

	is_new_issue(github, windows, Title, Labels) :-
		!,
		atomic_list_concat(['gh issue list --label ', Labels, ' --state open --search "', Title, ' in:title" | find "OPEN" > nul'], Command),
		\+ shell(Command).
	is_new_issue(github, _, Title, Labels) :-
		atomic_list_concat(['gh issue list --label ', Labels, ' --state open --search "', Title, ' in:title" | grep -q -s OPEN >/dev/null 2>&1'], Command),
		\+ shell(Command).

	is_new_issue(gitlab, windows, Title, Labels) :-
		!,
		atomic_list_concat(['glab issue list --label ', Labels, ' --in title --search "', Title, '" | find "No open issues match your search" > nul'], Command),
		shell(Command).
	is_new_issue(gitlab, _, Title, Labels) :-
		atomic_list_concat(['glab issue list --label ', Labels, ' --in title --search "', Title, '" | grep -q -s "No open issues match your search" >/dev/null 2>&1'], Command),
		shell(Command).

	create_bug_report(github, windows, Title, Description, Labels, Assignee) :-
		!,
		atomic_list_concat(['gh issue create --title \'', Title, '\' --body \'', Description, '\' --label ', Labels, ' --assignee ', Assignee, ' >/dev/null 2>&1'], Command),
		shell(Command).
	create_bug_report(github, _, Title, Description, Labels, Assignee) :-
		atomic_list_concat(['gh issue create --title \'', Title, '\' --body \'', Description, '\' --label ', Labels, ' --assignee ', Assignee, ' >/dev/null 2>&1'], Command),
		shell(Command).

	create_bug_report(gitlab, windows, Title, Description, Labels, Assignee) :-
		!,
		atomic_list_concat(['glab issue create --title \'', Title, '\' --description \'', Description, '\' --label ', Labels, ' --assignee ', Assignee, ' > nul'], Command),
		shell(Command).
	create_bug_report(gitlab, _, Title, Description, Labels, Assignee) :-
		atomic_list_concat(['glab issue create --title \'', Title, '\' --description \'', Description, '\' --label ', Labels, ' --assignee ', Assignee, ' >/dev/null 2>&1'], Command),
		shell(Command).

	title(Test, TestSet, Title) :-
		to_atom(Test, TestAtom),
		atomic_list_concat(['Test ', TestAtom, ' of test set ', TestSet, ' failed'], Title).

	description(Object, File, Position, Reason, Note, Time, Branch, Hash, Author, Date, Message, Description) :-
		to_atom(Object, ObjectAtom),
		(	tests_url(File, Position, URL) ->
			true
		;	to_atom(Position, PositionAtom),
			atomic_list_concat([File, ':', PositionAtom], URL)
		),
		(	failure_reason_to_atom(Reason, ReasonAtom) ->
			true
		;	to_atom(Reason, ReasonAtom)
		),
		(	Note == '' ->
			NoteAtom = '(none)'
		;	to_atom(Note, NoteAtom)
		),
		to_atom(Time, TimeAtom),
		to_atom(Author, AuthorAtom),
		to_atom(Date, DateAtom),
		to_atom(Message, MessageAtom),
		atomic_list_concat([
			'Test object: `', ObjectAtom, '`  \n',
			'Test file: ',  URL, '\n\n',
			'Failure:  \n',     ReasonAtom, '\n\n',
			'Note: ',  NoteAtom,  '\n\n',
			'Time: ',  TimeAtom, ' seconds\n\n',
			'Git branch: ',  Branch, '  \n',
			'Commit hash: ',  Hash, '  \n',
			'Commit author: ',  AuthorAtom, '  \n',
			'Commit date: ',  DateAtom, '  \n',
			'Commit message:  \n&emsp;',  MessageAtom, '\n'
		], Description).

	issue_server(Server) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(issue_server, Server0)},
		server(Server0, Server).

	tests_url(Short, Position, URL) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(tests_base_url, BaseURL)},
		BaseURL \== '',
		Position = Line-_,
		atomic_list_concat([BaseURL, Short, '#L', Line], URL).

	server('',     github).  % default
	server(github, github).
	server(gitlab, gitlab).

	to_atom(Term, Atom) :-
		(	atom(Term) ->
			Atom = Term
		;	numbervars(Term, 0, _),
			write_term_to_atom(Term, Atom, [numbervars(true), quoted(true)])
		).

	escape_double_quotes(Atom, EscapedAtom) :-
		atom_chars(Atom, Chars),
		escape_double_quotes_in_chars(Chars, EscapedChars),
		atom_chars(EscapedAtom, EscapedChars).

	escape_double_quotes_in_chars([], []).
	escape_double_quotes_in_chars(['\''| Chars], ['\'', '\\', '\'', '\''| EscapedChars]) :-
		!,
		escape_double_quotes_in_chars(Chars, EscapedChars).
	escape_double_quotes_in_chars([Char| Chars], [Char| EscapedChars]) :-
		escape_double_quotes_in_chars(Chars, EscapedChars).

	failure_reason_to_atom(success_instead_of_failure, '&emsp;test goal succeeded but should have failed').
	failure_reason_to_atom(success_instead_of_error(ExpectedError), ReasonAtom) :-
		to_atom(ExpectedError, ExpectedErrorAtom),
		atomic_list_concat(['&emsp;test goal succeeded but should have thrown an error:  \n', '&emsp;&emsp;expected `', ExpectedErrorAtom, '`'], ReasonAtom).

	failure_reason_to_atom(failure_instead_of_success, '&emsp;test goal failed but should have succeeded').
	failure_reason_to_atom(failure_instead_of_error(ExpectedError), ReasonAtom) :-
		to_atom(ExpectedError, ExpectedErrorAtom),
		atomic_list_concat(['&emsp;test goal failed but should have thrown an error:  \n', '&emsp;&emsp;expected `', ExpectedErrorAtom, '`'], ReasonAtom).

	failure_reason_to_atom(non_deterministic_success, '&emsp;test goal succeeded non-deterministically').

	failure_reason_to_atom(error_instead_of_failure(Error), ReasonAtom) :-
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test goal throws an error but should have failed: `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(assertion_error(Assertion, error(Error,_))), ReasonAtom) :-
		failure_reason_to_atom(error_instead_of_success(assertion_error(Assertion, Error)), ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(assertion_error(Assertion, Error)), ReasonAtom) :-
		to_atom(Assertion, AssertionAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test assertion throws an error: `', AssertionAtom, '` - `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(assertion_failure(Assertion)), ReasonAtom) :-
		to_atom(Assertion, AssertionAtom),
		atomic_list_concat(['&emsp;test assertion failed: `', AssertionAtom, '`'], ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(Error), ReasonAtom) :-
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test goal throws an error but should have succeeded: `', ErrorAtom, '`'], ReasonAtom).

	failure_reason_to_atom(wrong_error(ExpectedError, Error), ReasonAtom) :-
		to_atom(ExpectedError, ExpectedErrorAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test goal throws the wrong error:  \n', '&emsp;&emsp;expected `', ExpectedErrorAtom, '`  \n&emsp;&emsp;but got `', ErrorAtom, '`'], ReasonAtom).

	failure_reason_to_atom(quick_check_failed(Goal, Test, Shrinks, Seed), ReasonAtom) :-
		to_atom(Goal, GoalAtom),
		to_atom(Test, TestAtom),
		to_atom(Seed, SeedAtom),
		(	Shrinks == 1 ->
			atomic_list_concat(['&emsp;quick check test failure (at test `', TestAtom, '` after ', Shrinks, ' shrink with starting seed `', SeedAtom, '`): `', GoalAtom], ReasonAtom)
		;	atomic_list_concat(['&emsp;quick check test failure (at test `', TestAtom, '` after ', Shrinks, ' shrinks with starting seed `', SeedAtom, '`): `', GoalAtom], ReasonAtom)
		).

	failure_reason_to_atom(quick_check_error(error(Error,_), Goal, Test, Seed), ReasonAtom) :-
		failure_reason_to_atom(quick_check_error(Error, Goal, Test, Seed), ReasonAtom).
	failure_reason_to_atom(quick_check_error(Error, _Goal, Test, Seed), ReasonAtom) :-
		to_atom(Test, TestAtom),
		to_atom(Seed, SeedAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;quick check test error (at test `', TestAtom, '` with starting seed `', SeedAtom, '`): `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(quick_check_error(Error, Culprit), ReasonAtom) :-
		to_atom(Error, ErrorAtom),
		to_atom(Culprit, CulpritAtom),
		atomic_list_concat(['&emsp;quick check test error (caused by ', ErrorAtom, '): `', CulpritAtom, '`'], ReasonAtom).

	failure_reason_to_atom(step_error(Step, Error), ReasonAtom) :-
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;', Step, ' goal throws an error but should have succeeded: `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(step_failure(Step), ReasonAtom) :-
		atomic_list_concat(['&emsp;', Step, ' goal failed but should have succeeded'], ReasonAtom).

:- end_object.
