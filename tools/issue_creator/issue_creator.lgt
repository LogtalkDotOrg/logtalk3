%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
	% option to suppress the test file and directory path prefix
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
		version is 0:13:1,
		author is 'Paulo Moura',
		date is 2026-10-26,
		comment is 'Support for automatically creating bug report issues for failed tests in GitHub, GitLab, Azure DevOps, Gitea, or Jira servers.',
		remarks is [
			'Usage' - 'This tool is automatically loaded and used from the ``logtalk_tester`` automation script when using its ``-b`` option. See the script man page for details.'
		]
	]).

	:- uses(git, [
		branch/2, commit_hash_abbreviated/2, commit_author/2, commit_date/2, commit_message/2, commit_log/3
	]).

	:- uses(os, [
		decompose_file_name/3, operating_system_type/1, shell/1, environment_variable/2
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
	message_hook(Message) :-
		failure_details(Message, Object, Test, File, Position, Reason, Note, Time),
		create_bug_report(Object, Test, File, Position, Reason, Note, Time).

	failure_details(failed_test(Object, Test, File, Position, Reason, Flaky, Note, CPUTime, _WallTime), Object, Test, File, Position, Reason, Note, CPUTime) :-
		Flaky == false,
		\+ flaky_test_failure(Note).

	flaky_test_failure(Note) :-
		atom(Note),
		sub_atom(Note, _, _, _, flaky), !.

	create_bug_report(Object, Test, File, Position, Reason, Note, Time) :-
		decompose_file_name(File, Directory, _),
		branch(Directory, Branch0),
		(	Branch0 == 'HEAD',
			environment_variable('CI_COMMIT_REF_NAME', Branch) ->
			% assume GitLab CI/CD pipeline
			true
		;	Branch = Branch0
		),
		commit_hash_abbreviated(Directory, Hash),
		commit_author(Directory, Author),
		commit_date(Directory, Date),
		commit_message(Directory, Message),
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
		assignee(Server, Directory, Assignee),
		operating_system_type(OperatingSystem),
		title(Test, ShortDirectory, Title),
		% bypass the compiler as the flags are only created after loading this file
		{current_logtalk_flag(issue_labels, Labels)},
		is_new_issue(Server, OperatingSystem, Title, Labels),
		escape_double_quotes(Title, EscapedTitle),
		description(Server, Object, ShortFile, Position, Reason, Note, Time, Branch, Hash, Author, Date, Message, Description0),
		description_format(Server, Description0, Description),
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

	% Jira: list the summaries of the open bugs with the given labels that match the
	% title (using a phrase search) and look for the title; the "jira" CLI uses the
	% server, project, and credentials from its configuration (see "jira init")
	is_new_issue(jira, windows, Title, Labels) :-
		!,
		jira_list_command(windows, Title, Labels, ListCommand),
		shell_quote(windows, Title, QuotedTitle),
		atomic_list_concat([ListCommand, ' 2>nul | findstr /l /c:', QuotedTitle, ' > nul'], Command),
		\+ shell(Command).
	is_new_issue(jira, OperatingSystem, Title, Labels) :-
		jira_list_command(OperatingSystem, Title, Labels, ListCommand),
		shell_quote(OperatingSystem, Title, QuotedTitle),
		atomic_list_concat([ListCommand, ' 2>/dev/null | grep -q -s -F ', QuotedTitle, ' >/dev/null 2>&1'], Command),
		\+ shell(Command).

	% Gitea: list the titles of the open issues with the given labels that match the
	% title as a keyword and look for an exact title match (the "tea" CLI detects the
	% repository and login from the git remote of the current directory)
	is_new_issue(gitea, windows, Title, Labels) :-
		!,
		atomic_list_concat(['tea issues list --state open --labels ', Labels, ' --keyword "', Title, '" --fields title --output tsv 2>nul | findstr /x /l /c:"', Title, '" > nul'], Command),
		\+ shell(Command).
	is_new_issue(gitea, _, Title, Labels) :-
		atomic_list_concat(['tea issues list --state open --labels ', Labels, ' --keyword "', Title, '" --fields title --output tsv 2>/dev/null | grep -q -s -x -F "', Title, '" >/dev/null 2>&1'], Command),
		\+ shell(Command).

	% Azure DevOps: count the open (i.e. neither completed nor removed) bugs with
	% the same title that also have all the labels (which are Azure DevOps tags);
	% a new issue is reported only when the count is exactly zero; any failure
	% of the "az" command (e.g. not logged in) thus prevents creating issues
	is_new_issue(azure, windows, Title, Labels) :-
		!,
		azure_wiql(Title, Labels, WIQL),
		atomic_list_concat(['az boards query --wiql "', WIQL, '" --query "length(@)" --output tsv 2>nul | findstr /x "0" > nul'], Command),
		shell(Command).
	is_new_issue(azure, _, Title, Labels) :-
		azure_wiql(Title, Labels, WIQL),
		atomic_list_concat(['az boards query --wiql "', WIQL, '" --query "length(@)" --output tsv 2>/dev/null | grep -q -s -x 0 >/dev/null 2>&1'], Command),
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

	create_bug_report(jira, OperatingSystem, Title, Description, Labels, Assignee) :-
		% the assignee must be a user known to the Jira server; if assigning
		% the issue fails, retry without an assignee
		null_redirect(OperatingSystem, Redirect),
		(	jira_create_command(Title, Description, Labels, Assignee, Redirect, Command),
			shell(Command) ->
			true
		;	jira_create_command(Title, Description, Labels, '', Redirect, Command),
			shell(Command)
		).

	create_bug_report(gitea, OperatingSystem, Title, Description, Labels, Assignee) :-
		% the assignee must be a user with access to the repository; if
		% assigning the issue fails, retry without an assignee
		null_redirect(OperatingSystem, Redirect),
		(	gitea_create_command(Title, Description, Labels, Assignee, Redirect, Command),
			shell(Command) ->
			true
		;	gitea_create_command(Title, Description, Labels, '', Redirect, Command),
			shell(Command)
		).

	create_bug_report(azure, OperatingSystem, Title, Description, Labels, Assignee) :-
		% the assignee must be a known user in the organization; if
		% assigning the work item fails, retry without an assignee
		null_redirect(OperatingSystem, Redirect),
		labels_to_tags(Labels, Tags),
		(	azure_create_command(Title, Description, Tags, Assignee, Redirect, Command),
			shell(Command) ->
			true
		;	azure_create_command(Title, Description, Tags, '', Redirect, Command),
			shell(Command)
		).

	jira_create_command(Title, Description, Labels, Assignee, Redirect, Command) :-
		atom_chars(Labels, LabelsChars),
		split_labels(LabelsChars, LabelList),
		jira_label_options(LabelList, LabelOptions),
		(	Assignee == '' ->
			AssigneeOption = ''
		;	atomic_list_concat([' -a ', Assignee], AssigneeOption)
		),
		atomic_list_concat([
			'jira issue create -tBug -s \'', Title, '\' -b \'', Description, '\'',
			LabelOptions, AssigneeOption, ' --no-input ', Redirect
		], Command).

	jira_label_options([], '').
	jira_label_options([Label| Labels], Options) :-
		jira_label_options(Labels, Options0),
		atomic_list_concat([' -l ', Label, Options0], Options).

	jira_label_conditions([], '').
	jira_label_conditions([Label| Labels], Conditions) :-
		jira_label_conditions(Labels, Conditions0),
		atomic_list_concat([' AND labels = "', Label, '"', Conditions0], Conditions).

	% issue listing command using a JQL query (run by "jira" in the context of the
	% configured project) and printing only the summary column in plain mode
	jira_list_command(OperatingSystem, Title, Labels, Command) :-
		backslash_escape(Title, EscapedTitle),
		atom_chars(Labels, LabelsChars),
		split_labels(LabelsChars, LabelList),
		jira_label_conditions(LabelList, Conditions),
		atomic_list_concat([
			'issuetype = Bug AND statusCategory != Done', Conditions,
			' AND summary ~ "\\"', EscapedTitle, '\\""'
		], JQL),
		shell_quote(OperatingSystem, JQL, QuotedJQL),
		atomic_list_concat(['jira issue list --plain --no-headers --no-truncate --columns summary -q ', QuotedJQL], Command).

	shell_quote(windows, Text, Quoted) :-
		!,
		backslash_escape(Text, Escaped),
		atomic_list_concat(['"', Escaped, '"'], Quoted).
	shell_quote(_, Text, Quoted) :-
		escape_double_quotes(Text, Escaped),
		atomic_list_concat(['\'', Escaped, '\''], Quoted).

	% escape backslashes and double quotes with a backslash
	backslash_escape(Atom, Escaped) :-
		atom_chars(Atom, Chars),
		backslash_escape_chars(Chars, EscapedChars),
		atom_chars(Escaped, EscapedChars).

	backslash_escape_chars([], []).
	backslash_escape_chars([Char| Chars], Escaped) :-
		(	(Char == ('\\') ; Char == '"') ->
			Escaped = ['\\', Char| Escaped0]
		;	Escaped = [Char| Escaped0]
		),
		backslash_escape_chars(Chars, Escaped0).

	gitea_create_command(Title, Description, Labels, Assignee, Redirect, Command) :-
		(	Assignee == '' ->
			AssigneeOption = ''
		;	atomic_list_concat([' --assignees ', Assignee], AssigneeOption)
		),
		atomic_list_concat([
			'tea issues create --title \'', Title, '\' --description \'', Description, '\' --labels ', Labels,
			AssigneeOption, ' ', Redirect
		], Command).

	azure_create_command(Title, Description, Tags, Assignee, Redirect, Command) :-
		(	Assignee == '' ->
			AssigneeOption = ''
		;	atomic_list_concat([' --assigned-to ', Assignee], AssigneeOption)
		),
		atomic_list_concat([
			'az boards work-item create --type Bug --title \'', Title, '\'',
			' --fields \'Microsoft.VSTS.TCM.ReproSteps=', Description, '\' \'System.Tags=', Tags, '\'',
			AssigneeOption, ' --output none ', Redirect
		], Command).

	null_redirect(windows, '> nul 2>&1') :-
		!.
	null_redirect(_, '>/dev/null 2>&1').

	% Azure DevOps tags are separated by semicolons
	labels_to_tags(Labels, Tags) :-
		atom_chars(Labels, Chars),
		commas_to_semicolons(Chars, TagsChars),
		atom_chars(Tags, TagsChars).

	commas_to_semicolons([], []).
	commas_to_semicolons([Char0| Chars0], [Char| Chars]) :-
		(	Char0 == (',') ->
			Char = (';')
		;	Char = Char0
		),
		commas_to_semicolons(Chars0, Chars).

	% WIQL query for open bugs with a given title and given tags
	azure_wiql(Title, Labels, WIQL) :-
		double_single_quotes(Title, WIQLTitle),
		atom_chars(Labels, LabelsChars),
		split_labels(LabelsChars, LabelList),
		azure_tag_conditions(LabelList, Conditions),
		atomic_list_concat([
			'SELECT [System.Id] FROM WorkItems WHERE [System.WorkItemType] = \'Bug\'',
			' AND [System.StateCategory] NOT IN (\'Completed\', \'Removed\')',
			' AND [System.Title] = \'', WIQLTitle, '\'', Conditions
		], WIQL).

	azure_tag_conditions([], '').
	azure_tag_conditions([Label| Labels], Conditions) :-
		double_single_quotes(Label, WIQLLabel),
		azure_tag_conditions(Labels, Conditions0),
		atomic_list_concat([' AND [System.Tags] CONTAINS \'', WIQLLabel, '\'', Conditions0], Conditions).

	split_labels(Chars, [Label| Labels]) :-
		label_chars(Chars, LabelChars, Rest),
		atom_chars(Label, LabelChars),
		(	Rest == [] ->
			Labels = []
		;	split_labels(Rest, Labels)
		).

	label_chars([], [], []).
	label_chars([Char| Chars], LabelChars, Rest) :-
		(	Char == (',') ->
			LabelChars = [],
			Rest = Chars
		;	LabelChars = [Char| LabelChars0],
			label_chars(Chars, LabelChars0, Rest)
		).

	double_single_quotes(Atom, Doubled) :-
		atom_chars(Atom, Chars),
		double_single_quotes_in_chars(Chars, DoubledChars),
		atom_chars(Doubled, DoubledChars).

	double_single_quotes_in_chars([], []).
	double_single_quotes_in_chars([Char| Chars], Doubled) :-
		(	Char == '\'' ->
			Doubled = ['\'', '\''| Doubled0]
		;	Doubled = [Char| Doubled0]
		),
		double_single_quotes_in_chars(Chars, Doubled0).

	% Azure DevOps work item fields are HTML (not Markdown); convert the
	% (small) subset of Markdown used by the bug report description
	description_format(azure, Markdown, HTML) :-
		!,
		atom_chars(Markdown, Chars),
		html_chars(Chars, closed, HTMLChars),
		atom_chars(HTML, HTMLChars).
	% Jira: the "&emsp;" HTML entities used for indentation in the Markdown
	% descriptions are not supported
	description_format(jira, Markdown, Description) :-
		!,
		atom_chars(Markdown, Chars),
		strip_emsp(Chars, StrippedChars),
		atom_chars(Description, StrippedChars).
	description_format(_, Description, Description).

	strip_emsp([], []).
	strip_emsp(['&', 'e', 'm', 's', 'p', ';'| Chars], Stripped) :-
		!,
		strip_emsp(Chars, Stripped).
	strip_emsp([Char| Chars], [Char| Stripped]) :-
		strip_emsp(Chars, Stripped).

	html_chars([], closed, []) :-
		!.
	html_chars([], open, Chars) :-
		atom_chars('</code>', Chars).
	html_chars(['\n', '\n'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('<br><br>', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append_chars(Prefix, HTMLChars0, HTMLChars).
	html_chars(['\n'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('<br>', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append_chars(Prefix, HTMLChars0, HTMLChars).
	html_chars(['<'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('&lt;', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append_chars(Prefix, HTMLChars0, HTMLChars).
	html_chars(['>'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('&gt;', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append_chars(Prefix, HTMLChars0, HTMLChars).
	html_chars(['`'| Chars], closed, HTMLChars) :-
		!,
		atom_chars('<code>', Prefix),
		html_chars(Chars, open, HTMLChars0),
		append_chars(Prefix, HTMLChars0, HTMLChars).
	html_chars(['`'| Chars], open, HTMLChars) :-
		!,
		atom_chars('</code>', Prefix),
		html_chars(Chars, closed, HTMLChars0),
		append_chars(Prefix, HTMLChars0, HTMLChars).
	html_chars([Char| Chars], Code, [Char| HTMLChars]) :-
		html_chars(Chars, Code, HTMLChars).

	append_chars([], Chars, Chars).
	append_chars([Char| Chars0], Chars, [Char| Chars1]) :-
		append_chars(Chars0, Chars, Chars1).

	% GitHub and GitLab users are best identified by their login (as inferred
	% from the commit author email); Azure DevOps and Jira users by their email
	assignee(Server, Directory, Assignee) :-
		(	Server == azure
		;	Server == jira
		),
		!,
		commit_log(Directory, '%ae', Assignee).
	assignee(_, Directory, Assignee) :-
		commit_log(Directory, '%aL', Assignee).

	title(Test, TestSet, Title) :-
		to_atom(Test, TestAtom),
		atomic_list_concat(['Test ', TestAtom, ' of test set ', TestSet, ' failed'], Title).

	description(Server, Object, File, Position, Reason, Note, Time, Branch, Hash, Author, Date, Message, Description) :-
		to_atom(Object, ObjectAtom),
		(	tests_url(Server, File, Position, URL) ->
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

	% Azure Repos links use query parameters instead of a line anchor; the
	% base URL is expected to be the repository URL, optionally including the
	% version, e.g. https://dev.azure.com/org/project/_git/repo?version=GC<hash>
	tests_url(azure, Short, Position, URL) :-
		!,
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(tests_base_url, BaseURL)},
		BaseURL \== '',
		Position = Line-_,
		(	sub_atom(BaseURL, _, _, _, '?') ->
			Separator = ('&')
		;	Separator = ('?')
		),
		(	sub_atom(Short, 0, 1, _, '/') ->
			Path = Short
		;	atom_concat('/', Short, Path)
		),
		atomic_list_concat([BaseURL, Separator, 'path=', Path, '&line=', Line], URL).
	tests_url(_, Short, Position, URL) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(tests_base_url, BaseURL)},
		BaseURL \== '',
		Position = Line-_,
		atomic_list_concat([BaseURL, Short, '#L', Line], URL).

	server('',     github).  % default
	server(github, github).
	server(gitlab, gitlab).
	server(azure, azure).
	server(gitea, gitea).
	server(jira, jira).

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
		!,
		failure_reason_to_atom(error_instead_of_success(assertion_error(Assertion, Error)), ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(assertion_error(Assertion, Error)), ReasonAtom) :-
		!,
		numbervars((Assertion, Error), 0, _),
		to_atom(Assertion, AssertionAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test assertion throws an error: `', AssertionAtom, '` - `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(assertion_failure(Assertion)), ReasonAtom) :-
		!,
		to_atom(Assertion, AssertionAtom),
		atomic_list_concat(['&emsp;test assertion failed: `', AssertionAtom, '`'], ReasonAtom).
	failure_reason_to_atom(error_instead_of_success(Error), ReasonAtom) :-
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test goal throws an error but should have succeeded: `', ErrorAtom, '`'], ReasonAtom).

	failure_reason_to_atom(wrong_error(ExpectedError, Error), ReasonAtom) :-
		numbervars((ExpectedError, Error), 0, _),
		to_atom(ExpectedError, ExpectedErrorAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;test goal throws the wrong error:  \n', '&emsp;&emsp;expected `', ExpectedErrorAtom, '`  \n&emsp;&emsp;but got `', ErrorAtom, '`'], ReasonAtom).

	failure_reason_to_atom(quick_check_failed(Goal, Test, Shrinks, Seed, TestSeed), ReasonAtom) :-
		to_atom(Goal, GoalAtom),
		to_atom(Test, TestAtom),
		to_atom(Seed, SeedAtom),
		to_atom(TestSeed, TestSeedAtom),
		(	Shrinks == 1 ->
			atomic_list_concat(['&emsp;quick check test failure (at test `', TestAtom, '` after ', Shrinks, ' shrink with starting seed `', SeedAtom, '` and test seed `', TestSeedAtom, '`): `', GoalAtom, '`'], ReasonAtom)
		;	atomic_list_concat(['&emsp;quick check test failure (at test `', TestAtom, '` after ', Shrinks, ' shrinks with starting seed `', SeedAtom, '` and test seed `', TestSeedAtom, '`): `', GoalAtom, '`'], ReasonAtom)
		).

	failure_reason_to_atom(quick_check_error(error(Error,_), Goal, Test, Seed, TestSeed), ReasonAtom) :-
		!,
		failure_reason_to_atom(quick_check_error(Error, Goal, Test, Seed, TestSeed), ReasonAtom).
	failure_reason_to_atom(quick_check_error(Error, _Goal, Test, Seed, TestSeed), ReasonAtom) :-
		to_atom(Test, TestAtom),
		to_atom(Seed, SeedAtom),
		to_atom(TestSeed, TestSeedAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;quick check test error (at test `', TestAtom, '` with starting seed `', SeedAtom, '` and test seed `', TestSeedAtom, '`): `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(quick_check_error(Error, Culprit), ReasonAtom) :-
		numbervars((Error, Culprit), 0, _),
		to_atom(Error, ErrorAtom),
		to_atom(Culprit, CulpritAtom),
		atomic_list_concat(['&emsp;quick check test error (caused by ', ErrorAtom, '): `', CulpritAtom, '`'], ReasonAtom).

	failure_reason_to_atom(quick_check_broken(Why, Error), ReasonAtom) :-
		numbervars((Why, Error), 0, _),
		to_atom(Why, WhyAtom),
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;quick check test broken (caused by `', WhyAtom, '`): `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(quick_check_broken(Why), ReasonAtom) :-
		to_atom(Why, WhyAtom),
		atomic_list_concat(['&emsp;quick check test broken (caused by `', WhyAtom, '`)'], ReasonAtom).

	failure_reason_to_atom(step_error(Step, Error), ReasonAtom) :-
		to_atom(Error, ErrorAtom),
		atomic_list_concat(['&emsp;', Step, ' goal throws an error but should have succeeded: `', ErrorAtom, '`'], ReasonAtom).
	failure_reason_to_atom(step_failure(Step), ReasonAtom) :-
		atomic_list_concat(['&emsp;', Step, ' goal failed but should have succeeded'], ReasonAtom).

:- end_object.
