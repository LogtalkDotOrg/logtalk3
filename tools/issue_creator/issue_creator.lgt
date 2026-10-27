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
		version is 0:14:0,
		author is 'Paulo Moura',
		date is 2026-10-27,
		comment is 'Support for automatically creating bug report issues for failed tests in GitHub, GitLab, Azure DevOps, Gitea, Jira, or Linear servers.',
		remarks is [
			'Usage' - 'This tool is automatically loaded and used from the ``logtalk_tester`` automation script when using its ``-b`` option. See the script man page for details.'
		]
	]).

	:- uses(git, [
		branch/2, commit_hash_abbreviated/2, commit_author/2, commit_date/2, commit_message/2, commit_log/3
	]).

	:- uses(list, [
		append/3, member/2
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

	:- private(command_text/4).
	:- mode(command_text(+atom, +atom, +list(atom), -atom), zero_or_one).
	:- info(command_text/4, [
		comment is 'Renders an executable and literal arguments for the given operating system.',
		argnames is ['OperatingSystem', 'Executable', 'Arguments', 'Command']
	]).

	:- private(title/3).
	:- mode(title(+term, +atom, -atom), one).
	:- info(title/3, [
		comment is 'Constructs a stable title identifying the test and test set directory.',
		argnames is ['Test', 'Directory', 'Title']
	]).

	:- private(search_arguments/5).
	:- mode(search_arguments(+atom, +atom, +atom, -atom, -list(atom)), zero_or_one).
	:- info(search_arguments/5, [
		comment is 'Constructs a title and label search requesting at most one matching issue.',
		argnames is ['Server', 'Title', 'Labels', 'Executable', 'Arguments']
	]).

	:- private(search_command/4).
	:- mode(search_command(+atom, +atom, +atom, -atom), one).
	:- info(search_command/4, [
		comment is 'Constructs a command that succeeds only when the query succeeds and reports no matching issue.',
		argnames is ['OperatingSystem', 'Server', 'Query', 'Command']
	]).

	:- private(empty_search/2).
	:- mode(empty_search(+atom, -atom), one).
	:- info(empty_search/2, [
		comment is 'Returns the CLI output for a successful search with no matching issue.',
		argnames is ['Server', 'Output']
	]).

	:- private(jira_search_command/3).
	:- mode(jira_search_command(+atom, +atom, -atom), one).
	:- info(jira_search_command/3, [
		comment is 'Constructs a command accepting only the Jira CLI no-result diagnostic after an unsuccessful search.',
		argnames is ['OperatingSystem', 'Query', 'Command']
	]).

	:- private(jira_list_command/4).
	:- mode(jira_list_command(+atom, +atom, +atom, -atom), zero_or_one).
	:- info(jira_list_command/4, [
		comment is 'Constructs a command searching for at most one matching Jira bug.',
		argnames is ['OperatingSystem', 'Title', 'Labels', 'Command']
	]).

	:- private(jira_jql/3).
	:- mode(jira_jql(+atom, +atom, -atom), one).
	:- info(jira_jql/3, [
		comment is 'Constructs a JQL phrase search with an escaped title and validated labels.',
		argnames is ['Title', 'Labels', 'Query']
	]).

	:- private(jira_label_conditions/2).
	:- mode(jira_label_conditions(+list(atom), -atom), one).
	:- info(jira_label_conditions/2, [
		comment is 'Constructs a conjunction requiring every Jira label.',
		argnames is ['Labels', 'Conditions']
	]).

	:- private(azure_list_command/4).
	:- mode(azure_list_command(+atom, +atom, +atom, -atom), zero_or_one).
	:- info(azure_list_command/4, [
		comment is 'Constructs an Azure duplicate check using the configured project and its Bug workflow states.',
		argnames is ['OperatingSystem', 'Title', 'Labels', 'Command']
	]).

	:- private(azure_search_command/6).
	:- mode(azure_search_command(+atom, +atom, +atom, +atom, +atom, -atom), one).
	:- info(azure_search_command/6, [
		comment is 'Accepts absence only after successful project, workflow, and issue queries.',
		argnames is ['Shell', 'ProjectQuery', 'StatesQuery', 'IssueQuery', 'QuotedWIQL', 'Program']
	]).

	:- private(azure_states_query/1).
	:- mode(azure_states_query(-atom), one).
	:- info(azure_states_query/1, [
		comment is 'Returns a native Azure CLI query selecting nonterminal Bug state names as double-quoted WIQL literals.',
		argnames is ['Query']
	]).

	:- private(azure_wiql/3).
	:- mode(azure_wiql(+atom, +atom, -atom), one).
	:- info(azure_wiql/3, [
		comment is 'Constructs the title and tag portion of an Azure Bug query; project and workflow constraints are appended at execution.',
		argnames is ['Title', 'Labels', 'Query']
	]).

	:- private(powershell_command/2).
	:- mode(powershell_command(+atom, -atom), one).
	:- info(powershell_command/2, [
		comment is 'Encodes a PowerShell program to avoid cmd.exe expansion of query data.',
		argnames is ['Program', 'Command']
	]).


	:- private(valid_labels/1).
	:- mode(valid_labels(+atom), zero_or_one).
	:- info(valid_labels/1, [
		comment is 'Checks for comma-separated nonempty ASCII labels starting with a letter or digit and containing only letters, digits, underscores, or hyphens.',
		argnames is ['Labels']
	]).

	:- private(valid_label_code/1).
	:- mode(valid_label_code(+character_code), zero_or_one).
	:- info(valid_label_code/1, [
		comment is 'Checks for an ASCII letter, digit, underscore, or hyphen.',
		argnames is ['Code']
	]).

	search_arguments(github, Title, Labels, gh, [issue, list, '--state', open, '--label', Labels, '--search', Search, '--limit', '1', '--json', number, '--jq', length]) :-
		backslash_escape(Title, Escaped),
		atomic_list_concat(['"', Escaped, '" in:title'], Search).
	search_arguments(gitlab, Title, Labels, glab, [issue, list, '--label', Labels, '--search', Title, '--in', title, '--per-page', '1', '--output', json, '--jq', length]).
	search_arguments(gitea, Title, Labels, tea, [issues, list, '--state', open, '--labels', Labels, '--keyword', Title, '--fields', index, '--output', simple, '--limit', '1']).
	search_arguments(linear, Title, Labels, linear, Arguments) :-
		label_arguments(Labels, Options),
		Arguments = [issue, query, '--search', Title, '--state', triage, '--state', backlog, '--state', unstarted, '--state', started, '--limit', '1', '--json', '--no-pager', '--no-input'| Options].

	search_command(windows, Server, Query, Command) :-
		!,
		empty_search(Server, Empty),
		(	Empty == '' ->
			Check = 'findstr /l /x /c:"1:__logtalk_query_ok__" >nul'
		;	atomic_list_concat([
				'findstr /l /x /c:"1:', Empty, '" /c:"2:__logtalk_query_ok__"',
				' | find /c /v "" | findstr /x /c:"2" >nul'
			], Check)
		),
		atomic_list_concat(['(', Query, ' 2>nul && echo __logtalk_query_ok__) | findstr /n "^" | ', Check], Command).
	search_command(_, Server, Query, Command) :-
		empty_search(Server, Empty),
		shell_quote(unix, Empty, Quoted),
		atomic_list_concat(['Result=$(', Query, ' 2>/dev/null) && test "$Result" = ', Quoted], Command).

	empty_search(github, '0').
	empty_search(gitlab, '0').
	empty_search(gitea, '').
	empty_search(linear, '[]').

	jira_search_command(windows, Query, Command) :-
		!,
		atomic_list_concat([
			'(', Query, ' 2>&1 1>nul || echo __logtalk_query_failed__)',
			' | findstr /n "^"',
			' | findstr /r /c:"^1:.*No result found for given query in project " /c:"^2:__logtalk_query_failed__$"',
			' | find /c /v "" | findstr /x /c:"2" >nul'
		], Command).
	jira_search_command(_, Query, Command) :-
		atomic_list_concat([
			'Result=$(', Query, ' 2>&1 >/dev/null); Status=$?; ',
			'test "$Status" -eq 1 && printf "%s\\n" "$Result"',
			' | grep -q -s -E \'^.*No result found for given query in project "[^"]*"$\''
		], Command).

	:- private(command_arguments/3).
	:- mode(command_arguments(+atom, +list(atom), -atom), zero_or_one).
	:- info(command_arguments/3, [
		comment is 'Renders individually quoted command arguments.',
		argnames is ['OperatingSystem', 'Arguments', 'Text']
	]).

	:- private(run_cli/3).
	:- mode(run_cli(+atom, +atom, +list(atom)), zero_or_one_or_error).
	:- info(run_cli/3, [
		comment is 'Runs a command, discarding its output and succeeding only on a successful exit status.',
		argnames is ['OperatingSystem', 'Executable', 'Arguments'],
		exceptions is ['An operating-system operation raises an error' - error(system_error, 'Context')]
	]).

	:- private(null_redirect/2).
	:- mode(null_redirect(+atom, -atom), one).
	:- info(null_redirect/2, [
		comment is 'Returns the operating-system redirection for discarding standard output and standard error.',
		argnames is ['OperatingSystem', 'Redirect']
	]).

	:- private(create_arguments/7).
	:- mode(create_arguments(+atom, +atom, +atom, +atom, +atom, -atom, -list(atom)), one).
	:- info(create_arguments/7, [
		comment is 'Constructs literal issue creation arguments for a tracker.',
		argnames is ['Server', 'Title', 'Description', 'Labels', 'Assignee', 'Executable', 'Arguments']
	]).

	:- private(option_arguments/3).
	:- mode(option_arguments(+atom, +atom, -list(atom)), one).
	:- info(option_arguments/3, [
		comment is 'Omits an option when its value is empty.',
		argnames is ['Option', 'Value', 'Arguments']
	]).

	:- private(label_arguments/2).
	:- mode(label_arguments(+atom, -list(atom)), one).
	:- info(label_arguments/2, [
		comment is 'Constructs repeated label arguments from comma-separated labels.',
		argnames is ['Labels', 'Arguments']
	]).

	:- private(repeated_labels/2).
	:- mode(repeated_labels(+list(atom), -list(atom)), one).
	:- info(repeated_labels/2, [
		comment is 'Pairs each label with its command option.',
		argnames is ['Labels', 'Arguments']
	]).

	create_arguments(github, Title, Description, Labels, Assignee, gh, Arguments) :-
		option_arguments('--assignee', Assignee, Assignment),
		Arguments = [issue, create, '--title', Title, '--body', Description, '--label', Labels| Assignment].
	create_arguments(gitlab, Title, Description, Labels, Assignee, glab, Arguments) :-
		option_arguments('--assignee', Assignee, Assignment),
		Arguments = [issue, create, '--title', Title, '--description', Description, '--label', Labels, '--yes'| Assignment].
	create_arguments(gitea, Title, Description, Labels, Assignee, tea, Arguments) :-
		option_arguments('--assignees', Assignee, Assignment),
		Arguments = [issues, create, '--title', Title, '--description', Description, '--labels', Labels| Assignment].
	create_arguments(azure, Title, Description, Labels, Assignee, az, Arguments) :-
		azure_labels_to_tags(Labels, Tags),
		atom_concat('Microsoft.VSTS.TCM.ReproSteps=', Description, ReproSteps),
		atom_concat('System.Tags=', Tags, TagField),
		option_arguments('--assigned-to', Assignee, Assignment),
		Arguments = [boards, 'work-item', create, '--type', 'Bug', '--title', Title, '--fields', ReproSteps, TagField, '--output', none| Assignment].
	create_arguments(jira, Title, Description, Labels, Assignee, jira, Arguments) :-
		label_arguments(Labels, LabelArguments),
		option_arguments('-a', Assignee, Assignment),
		append(LabelArguments, Assignment, Options),
		Arguments = [issue, create, '-tBug', '-s', Title, '-b', Description, '--no-input'| Options].
	create_arguments(linear, Title, Description, Labels, Assignee, linear, Arguments) :-
		label_arguments(Labels, LabelArguments),
		option_arguments('--assignee', Assignee, Assignment),
		append(LabelArguments, Assignment, Options),
		Arguments = [issue, create, '--title', Title, '--description', Description, '--yes', '--no-input'| Options].

	option_arguments(Option, Value, Arguments) :-
		(	Value == '' ->
			Arguments = []
		;	Arguments = [Option, Value]
		).

	label_arguments(Labels, Arguments) :-
		atom_chars(Labels, Characters),
		split_labels(Characters, LabelList),
		repeated_labels(LabelList, Arguments).

	repeated_labels([], []).
	repeated_labels([Label| Labels], ['-l', Label| Arguments]) :-
		repeated_labels(Labels, Arguments).

	command_text(OperatingSystem, Executable, Arguments, Command) :-
		shell_quote(OperatingSystem, Executable, QuotedExecutable),
		command_arguments(OperatingSystem, Arguments, Text),
		atom_concat(QuotedExecutable, Text, Command).

	command_arguments(_, [], '').
	command_arguments(OperatingSystem, [Argument| Arguments], Text) :-
		shell_quote(OperatingSystem, Argument, Quoted),
		command_arguments(OperatingSystem, Arguments, Tail),
		atomic_list_concat([' ', Quoted, Tail], Text).

	run_cli(OperatingSystem, Executable, Arguments) :-
		command_text(OperatingSystem, Executable, Arguments, Command0),
		null_redirect(OperatingSystem, Redirect),
		atomic_list_concat([Command0, ' ', Redirect], Command),
		shell(Command).

	null_redirect(windows, '>nul 2>&1') :-
		!.
	null_redirect(_, '>/dev/null 2>&1').

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
		{current_logtalk_flag(issue_labels, Labels)},
		valid_labels(Labels),
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
		is_new_issue(Server, OperatingSystem, Title, Labels),
		description(Server, Object, ShortFile, Position, Reason, Note, Time, Branch, Hash, Author, Date, Message, Description0),
		description_format(Server, Description0, Description),
		create_bug_report(Server, OperatingSystem, Title, Description, Labels, Assignee).

	is_new_issue(azure, OperatingSystem, Title, Labels) :-
		!,
		azure_list_command(OperatingSystem, Title, Labels, Command),
		shell(Command).
	is_new_issue(jira, OperatingSystem, Title, Labels) :-
		jira_list_command(OperatingSystem, Title, Labels, ListCommand),
		jira_search_command(OperatingSystem, ListCommand, Command),
		shell(Command).
	is_new_issue(Server, OperatingSystem, Title, Labels) :-
		search_arguments(Server, Title, Labels, Executable, Arguments),
		command_text(OperatingSystem, Executable, Arguments, Query),
		search_command(OperatingSystem, Server, Query, Command),
		shell(Command).

	create_bug_report(Server, OperatingSystem, Title, Description, Labels, Assignee) :-
		create_arguments(Server, Title, Description, Labels, Assignee, Executable, Arguments),
		(	run_cli(OperatingSystem, Executable, Arguments) ->
			true
		;	Assignee \== '',
			member(Server, [azure, gitea, jira, linear]),
			create_arguments(Server, Title, Description, Labels, '', Executable, Unassigned),
			run_cli(OperatingSystem, Executable, Unassigned)
		).

	jira_label_conditions([], '').
	jira_label_conditions([Label| Labels], Conditions) :-
		jira_label_conditions(Labels, Conditions0),
		atomic_list_concat([' AND labels = "', Label, '"', Conditions0], Conditions).

	jira_list_command(OperatingSystem, Title, Labels, Command) :-
		jira_jql(Title, Labels, JQL),
		command_text(OperatingSystem, jira, [issue, list, '--plain', '--no-headers', '--no-truncate', '--columns', summary, '--paginate', '1', '-q', JQL], Command).

	jira_jql(Title, Labels, JQL) :-
		backslash_escape(Title, EscapedTitle),
		atomic_list_concat(['"', EscapedTitle, '"'], Phrase),
		backslash_escape(Phrase, EscapedPhrase),
		atom_chars(Labels, LabelsChars),
		split_labels(LabelsChars, LabelList),
		jira_label_conditions(LabelList, Conditions),
		atomic_list_concat([
			'issuetype = Bug AND statusCategory != Done', Conditions,
			' AND summary ~ "', EscapedPhrase, '"'
		], JQL).

	shell_quote(windows, Text, Quoted) :-
		!,
		backslash_escape(Text, Escaped),
		atomic_list_concat(['"', Escaped, '"'], Quoted).
	shell_quote(powershell, Text, Quoted) :-
		!,
		double_single_quotes(Text, Escaped),
		atomic_list_concat(['\'', Escaped, '\''], Quoted).
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

	% Azure DevOps tags are separated by semicolons
	azure_labels_to_tags(Labels, Tags) :-
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

	azure_list_command(OperatingSystem, Title, Labels, Command) :-
		( 	OperatingSystem == windows ->
			Shell = powershell
		; 	Shell = unix
		),
		command_text(Shell, az, [devops, project, show, '--query', 'type(name) == \'string\' && name != \'\' && !contains(name, \'"\') && name || \'\'', '--output', tsv], ProjectQuery),
		azure_states_query(StatesFilter),
		command_text(Shell, az, [devops, invoke, '--area', wit, '--resource', workItemTypeStates, '--api-version', '7.1', '--query', StatesFilter, '--output', tsv, '--route-parameters', 'type=Bug'], StatesQuery),
		command_text(Shell, az, [boards, query, '--query', 'length(@ || `[]`)', '--output', tsv, '--wiql'], IssueQuery),
		azure_wiql(Title, Labels, WIQL),
		shell_quote(Shell, WIQL, QuotedWIQL),
		azure_search_command(Shell, ProjectQuery, StatesQuery, IssueQuery, QuotedWIQL, Program),
		( 	Shell == powershell ->
			powershell_command(Program, Command)
		; 	Command = Program
		).

	azure_states_query(Query) :-
		atomic_list_concat([
			'length(value) > `0` && ',
			'length(value[?type(name) != \'string\' || name == \'\' || contains(name, \'"\') || type(category) != \'string\' || category == \'\']) == `0` && ',
			'join(\',\', value[?category != \'Completed\' && category != \'Removed\'].join(\'\', [\'"\', name, \'"\'])) || \'\''
		], Query).

	azure_search_command(powershell, ProjectQuery, StatesQuery, IssueQuery, WIQL, Program) :-
		!,
		atomic_list_concat([
			'$ErrorActionPreference = \'Stop\'; ',
			'$Project = & ', ProjectQuery, ' 2>$null; ',
			'if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($Project)) { exit 1 }; ',
			'$States = & ', StatesQuery, ' (\'project=\' + $Project) 2>$null; ',
			'if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($States)) { exit 1 }; ',
			'$WIQL = ', WIQL, ' + \' AND [System.TeamProject] = "\' + $Project + \'" AND [System.State] IN (\' + $States + \')\'; ',
			'$Result = & ', IssueQuery, ' $WIQL 2>$null; ',
			'if ($LASTEXITCODE -eq 0 -and (!$Result -or $Result -eq \'0\')) { exit 0 }; exit 1'
		], Program).
	azure_search_command(_, ProjectQuery, StatesQuery, IssueQuery, WIQL, Program) :-
		atomic_list_concat([
			'Project=$(', ProjectQuery, ' 2>/dev/null) && test -n "$Project" && ',
			'States=$(', StatesQuery, ' "project=$Project" 2>/dev/null) && test -n "$States" && ',
			'WIQL=', WIQL, ' && ',
			'Result=$(', IssueQuery, ' "$WIQL AND [System.TeamProject] = \\"$Project\\" AND [System.State] IN ($States)" 2>/dev/null) && ',
			'{ test "$Result" = 0 || test -z "$Result"; }'
		], Program).

	powershell_command(Program, Command) :-
		atom_codes(Program, Codes),
		utf_16le::codes_to_bytes(Codes, Bytes),
		base64::generate(atom(Encoded), Bytes),
		atom_concat('powershell -NoLogo -NoProfile -NonInteractive -EncodedCommand ', Encoded, Command).

	azure_wiql(Title, Labels, WIQL) :-
		double_single_quotes(Title, WIQLTitle),
		atom_chars(Labels, LabelsChars),
		split_labels(LabelsChars, LabelList),
		azure_tag_conditions(LabelList, Conditions),
		atomic_list_concat([
			'SELECT [System.Id] FROM WorkItems WHERE [System.WorkItemType] = \'Bug\'',
			' AND [System.Title] = \'', WIQLTitle, '\'', Conditions
		], WIQL).

	azure_tag_conditions([], '').
	azure_tag_conditions([Label| Labels], Conditions) :-
		azure_tag_conditions(Labels, Conditions0),
		atomic_list_concat([' AND [System.Tags] CONTAINS \'', Label, '\'', Conditions0], Conditions).

	valid_labels(Labels) :-
		atom(Labels),
		\+ sub_atom(Labels, _, 1, 0, ','),
		atom_chars(Labels, Chars),
		split_labels(Chars, LabelList),
		forall(
			member(Label, LabelList),
			(
				atom_codes(Label, [First| Codes]),
				First =\= 0'_,
				First =\= 0'-,
				forall(member(Code, [First| Codes]), valid_label_code(Code))
			)
		).

	valid_label_code(Code) :-
		Code >= 0'0,
		Code =< 0'9,
		!.
	valid_label_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z,
		!.
	valid_label_code(Code) :-
		Code >= 0'a,
		Code =< 0'z,
		!.
	valid_label_code(0'_).
	valid_label_code(0'-).

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
		append(Prefix, HTMLChars0, HTMLChars).
	html_chars(['\n'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('<br>', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append(Prefix, HTMLChars0, HTMLChars).
	html_chars(['<'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('&lt;', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append(Prefix, HTMLChars0, HTMLChars).
	html_chars(['>'| Chars], Code, HTMLChars) :-
		!,
		atom_chars('&gt;', Prefix),
		html_chars(Chars, Code, HTMLChars0),
		append(Prefix, HTMLChars0, HTMLChars).
	html_chars(['`'| Chars], closed, HTMLChars) :-
		!,
		atom_chars('<code>', Prefix),
		html_chars(Chars, open, HTMLChars0),
		append(Prefix, HTMLChars0, HTMLChars).
	html_chars(['`'| Chars], open, HTMLChars) :-
		!,
		atom_chars('</code>', Prefix),
		html_chars(Chars, closed, HTMLChars0),
		append(Prefix, HTMLChars0, HTMLChars).
	html_chars([Char| Chars], Code, [Char| HTMLChars]) :-
		html_chars(Chars, Code, HTMLChars).

	% GitHub and GitLab users are best identified by their login (as inferred
	% from the commit author email); Azure DevOps, Jira, and Linear users by their email
	assignee(Server, Directory, Assignee) :-
		(	Server == azure
		;	Server == jira
		;	Server == linear
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
	server(linear, linear).

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
