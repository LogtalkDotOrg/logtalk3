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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-27,
		comment is 'Unit tests for the "issue_creator" tool.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(issue_creator).

	test(issue_creator_expansion_reporting_callback, deterministic(Expanded == [])) :-
		issue_creator_test_expansion::term_expansion((logtalk::message_hook(Message, _, lgtunit, _) :- message_hook(Message), fail), Expanded).

	test(issue_creator_expansion_other_component, false) :-
		issue_creator_test_expansion::term_expansion((logtalk::message_hook(_, _, other, _) :- true), _).

	test(issue_creator_expansion_local_predicate, false) :-
		issue_creator_test_expansion::term_expansion((message_hook(_) :- true), _).

	test(issue_creator_failure_details, deterministic(Details == [suite, sample, 'tests.lgt', 10-12, failure_instead_of_success, 'needs review', 2])) :-
		issue_creator<<failure_details(failed_test(suite, sample, 'tests.lgt', 10-12, failure_instead_of_success, false, 'needs review', 2, 5), Object, Test, File, Position, Reason, Note, Time),
		Details = [Object, Test, File, Position, Reason, Note, Time].

	test(issue_creator_failure_details_nondeterministic, deterministic(Reason == non_deterministic_success)) :-
		issue_creator<<failure_details(failed_test(suite, sample, 'tests.lgt', 10-12, non_deterministic_success, false, '', 2, 5), _, _, _, _, Reason, _, _).

	test(issue_creator_failure_details_flaky_flag, false) :-
		issue_creator<<failure_details(failed_test(suite, sample, 'tests.lgt', 10-12, failure_instead_of_success, true, '', 2, 5), _, _, _, _, _, _, _).

	test(issue_creator_failure_details_flaky_note, false) :-
		issue_creator<<failure_details(failed_test(suite, sample, 'tests.lgt', 10-12, failure_instead_of_success, false, 'known flaky test', 2, 5), _, _, _, _, _, _, _).

	test(issue_creator_failure_details_passed, false) :-
		issue_creator<<failure_details(passed_test(suite, sample, 'tests.lgt', 10-12, false, '', 2, 5), _, _, _, _, _, _, _).

	test(issue_creator_failure_details_obsolete, false) :-
		issue_creator<<failure_details(failed_test(suite, sample, 'tests.lgt', 10-12, failure_instead_of_success, '', 2), _, _, _, _, _, _, _).

	test(issue_creator_message_hook_flaky_flag, false) :-
		issue_creator<<message_hook(failed_test(suite, sample, 'tests.lgt', 10-12, failure_instead_of_success, true, '', 2, 5)).

	test(issue_creator_message_hook_flaky_note, false) :-
		issue_creator<<message_hook(failed_test(suite, sample, 'tests.lgt', 10-12, non_deterministic_success, false, 'known flaky test', 2, 5)).

	test(issue_creator_title_atomic, deterministic(Title == 'Test sample of test set suite/ failed')) :-
		issue_creator<<title(sample, 'suite/', Title).

	test(issue_creator_quick_check_failed_no_shrinks, deterministic(Atom == '&emsp;quick check test failure (at test `3` after 0 shrinks with starting seed `123` and test seed `456`): `sample(a)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_failed(sample(a), 3, 0, 123, 456), Atom).

	test(issue_creator_quick_check_failed_one_shrink, deterministic(Atom == '&emsp;quick check test failure (at test `3` after 1 shrink with starting seed `123` and test seed `456`): `sample(a)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_failed(sample(a), 3, 1, 123, 456), Atom).

	test(issue_creator_quick_check_failed_many_shrinks, deterministic(Atom == '&emsp;quick check test failure (at test `3` after 2 shrinks with starting seed `123` and test seed `456`): `sample(a)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_failed(sample(a), 3, 2, 123, 456), Atom).

	test(issue_creator_quick_check_error, deterministic(Atom == '&emsp;quick check test error (at test `3` with starting seed `123` and test seed `456`): `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_error(instantiation_error, sample(a), 3, 123, 456), Atom).

	test(issue_creator_quick_check_error_wrapped, deterministic(Atom == '&emsp;quick check test error (at test `3` with starting seed `123` and test seed `456`): `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_error(error(instantiation_error, context), sample(a), 3, 123, 456), Atom).

	test(issue_creator_reason_success_instead_of_failure, deterministic(Atom == '&emsp;test goal succeeded but should have failed')) :-
		issue_creator<<failure_reason_to_atom(success_instead_of_failure, Atom).

	test(issue_creator_reason_success_instead_of_error, deterministic(Atom == '&emsp;test goal succeeded but should have thrown an error:  \n&emsp;&emsp;expected `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(success_instead_of_error(instantiation_error), Atom).

	test(issue_creator_reason_failure_instead_of_success, deterministic(Atom == '&emsp;test goal failed but should have succeeded')) :-
		issue_creator<<failure_reason_to_atom(failure_instead_of_success, Atom).

	test(issue_creator_reason_failure_instead_of_error, deterministic(Atom == '&emsp;test goal failed but should have thrown an error:  \n&emsp;&emsp;expected `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(failure_instead_of_error(instantiation_error), Atom).

	test(issue_creator_reason_nondeterministic, deterministic(Atom == '&emsp;test goal succeeded non-deterministically')) :-
		issue_creator<<failure_reason_to_atom(non_deterministic_success, Atom).

	test(issue_creator_reason_error_instead_of_failure, deterministic(Atom == '&emsp;test goal throws an error but should have failed: `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_failure(instantiation_error), Atom).

	test(issue_creator_reason_assertion_error_wrapped, deterministic(Atom == '&emsp;test assertion throws an error: `sample(a)` - `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(assertion_error(sample(a), error(instantiation_error, context))), Atom).

	test(issue_creator_reason_assertion_error, deterministic(Atom == '&emsp;test assertion throws an error: `sample(a)` - `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(assertion_error(sample(a), instantiation_error)), Atom).

	test(issue_creator_reason_assertion_failure, deterministic(Atom == '&emsp;test assertion failed: `sample(a)`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(assertion_failure(sample(a))), Atom).

	test(issue_creator_reason_error_instead_of_success, deterministic(Atom == '&emsp;test goal throws an error but should have succeeded: `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(instantiation_error), Atom).

	test(issue_creator_reason_wrong_error, deterministic(Atom == '&emsp;test goal throws the wrong error:  \n&emsp;&emsp;expected `instantiation_error`  \n&emsp;&emsp;but got `type_error(atom,1)`')) :-
		issue_creator<<failure_reason_to_atom(wrong_error(instantiation_error, type_error(atom, 1)), Atom).

	test(issue_creator_reason_wrong_error_distinct_variables, deterministic(Atom == '&emsp;test goal throws the wrong error:  \n&emsp;&emsp;expected `expected(A)`  \n&emsp;&emsp;but got `actual(B)`')) :-
		issue_creator<<failure_reason_to_atom(wrong_error(expected(_), actual(_)), Atom).

	test(issue_creator_reason_wrong_error_shared_variables, deterministic(Atom == '&emsp;test goal throws the wrong error:  \n&emsp;&emsp;expected `expected(A,B)`  \n&emsp;&emsp;but got `actual(A,C)`')) :-
		issue_creator<<failure_reason_to_atom(wrong_error(expected(Shared, _), actual(Shared, _)), Atom).

	test(issue_creator_reason_assertion_distinct_variables, deterministic(Atom == '&emsp;test assertion throws an error: `check(A)` - `problem(B)`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(assertion_error(check(_), problem(_))), Atom).

	test(issue_creator_reason_assertion_shared_variables, deterministic(Atom == '&emsp;test assertion throws an error: `check(A)` - `problem(A)`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(assertion_error(check(Shared), problem(Shared))), Atom).

	test(issue_creator_reason_assertion_wrapped_variables, deterministic(Atom == '&emsp;test assertion throws an error: `check(A)` - `problem(B)`')) :-
		issue_creator<<failure_reason_to_atom(error_instead_of_success(assertion_error(check(_), error(problem(_), context))), Atom).

	test(issue_creator_reason_quick_check_culprit, deterministic(Atom == '&emsp;quick check test error (caused by instantiation_error): `sample(a)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_error(instantiation_error, sample(a)), Atom).

	test(issue_creator_reason_quick_check_culprit_variables, deterministic(Atom == '&emsp;quick check test error (caused by condition(A)): `problem(B)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_error(condition(_), problem(_)), Atom).

	test(issue_creator_reason_quick_check_broken_template, deterministic(Atom == '&emsp;quick check test broken (caused by `template_error(42)`): `error(type_error(callable,42),context)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(template_error(42), error(type_error(callable, 42), context)), Atom).

	test(issue_creator_reason_quick_check_broken_generator_error, deterministic(Atom == '&emsp;quick check test broken (caused by `generate_test_error(sample)`): `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(generate_test_error(sample), instantiation_error), Atom).

	test(issue_creator_reason_quick_check_broken_generator_failure, deterministic(Atom == '&emsp;quick check test broken (caused by `generate_test_failure(sample)`): `integer`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(generate_test_failure(sample), integer), Atom).

	test(issue_creator_reason_quick_check_broken_label_error, deterministic(Atom == '&emsp;quick check test broken (caused by `label_goal_error(label)`): `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(label_goal_error(label), instantiation_error), Atom).

	test(issue_creator_reason_quick_check_broken_label_failure, deterministic(Atom == '&emsp;quick check test broken (caused by `label_goal_failure(label)`)')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(label_goal_failure(label)), Atom).

	test(issue_creator_reason_quick_check_broken_condition_error, deterministic(Atom == '&emsp;quick check test broken (caused by `pre_condition_error(condition)`): `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(pre_condition_error(condition), instantiation_error), Atom).

	test(issue_creator_reason_quick_check_broken_condition_failure, deterministic(Atom == '&emsp;quick check test broken (caused by `pre_condition_always_fails(condition)`)')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(pre_condition_always_fails(condition)), Atom).

	test(issue_creator_reason_quick_check_broken_variables, deterministic(Atom == '&emsp;quick check test broken (caused by `condition(A,B)`): `problem(A,C)`')) :-
		issue_creator<<failure_reason_to_atom(quick_check_broken(condition(Shared, _), problem(Shared, _)), Atom).

	test(issue_creator_reason_step_error, deterministic(Atom == '&emsp;setup goal throws an error but should have succeeded: `instantiation_error`')) :-
		issue_creator<<failure_reason_to_atom(step_error(setup, instantiation_error), Atom).

	test(issue_creator_reason_step_failure, deterministic(Atom == '&emsp;cleanup goal failed but should have succeeded')) :-
		issue_creator<<failure_reason_to_atom(step_failure(cleanup), Atom).

	test(issue_creator_description_quick_check_failure, true) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<description(github, suite, 'tests.lgt', 10-12, quick_check_failed(sample(a), 3, 1, 123, 456), '', 1, main, abc123, author, date, message, Description)
		),
		assertion(sub_atom(Description, _, _, _, 'Failure:  \n&emsp;quick check test failure (at test `3` after 1 shrink with starting seed `123` and test seed `456`): `sample(a)`\n\n')).

	test(issue_creator_description_quick_check_error, true) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<description(github, suite, 'tests.lgt', 10-12, quick_check_error(error(instantiation_error, context), sample(a), 3, 123, 456), '', 1, main, abc123, author, date, message, Description)
		),
		assertion(sub_atom(Description, _, _, _, 'Failure:  \n&emsp;quick check test error (at test `3` with starting seed `123` and test seed `456`): `instantiation_error`\n\n')).

	test(issue_creator_description_quick_check_broken_error, true) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<description(github, suite, 'tests.lgt', 10-12, quick_check_broken(generate_test_error(sample), instantiation_error), '', 1, main, abc123, author, date, message, Description)
		),
		assertion(sub_atom(Description, _, _, _, 'Failure:  \n&emsp;quick check test broken (caused by `generate_test_error(sample)`): `instantiation_error`\n\n')).

	test(issue_creator_description_quick_check_broken_failure, true) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<description(github, suite, 'tests.lgt', 10-12, quick_check_broken(label_goal_failure(label)), '', 1, main, abc123, author, date, message, Description)
		),
		assertion(sub_atom(Description, _, _, _, 'Failure:  \n&emsp;quick check test broken (caused by `label_goal_failure(label)`)\n\n')).

	test(issue_creator_format_azure_empty, deterministic(HTML == '')) :-
		issue_creator<<description_format(azure, '', HTML).

	test(issue_creator_format_azure_plain, deterministic(HTML == 'plain text')) :-
		issue_creator<<description_format(azure, 'plain text', HTML).

	test(issue_creator_format_azure_newlines, deterministic(HTML == 'one<br>two<br><br>three<br><br><br>four')) :-
		issue_creator<<description_format(azure, 'one\ntwo\n\nthree\n\n\nfour', HTML).

	test(issue_creator_format_azure_code, deterministic(HTML == 'before <code>first</code> and <code>second</code> after')) :-
		issue_creator<<description_format(azure, 'before `first` and `second` after', HTML).

	test(issue_creator_format_azure_unclosed_code, deterministic(HTML == 'before <code>unfinished</code>')) :-
		issue_creator<<description_format(azure, 'before `unfinished', HTML).

	test(issue_creator_format_azure_empty_code, deterministic(HTML == '<code></code>')) :-
		issue_creator<<description_format(azure, '``', HTML).

	test(issue_creator_format_azure_escape, deterministic(HTML == '&lt;tag&gt; <code>a &lt; b<br>c &gt; d</code>')) :-
		issue_creator<<description_format(azure, '<tag> `a < b\nc > d`', HTML).

	test(issue_creator_format_azure_indentation, deterministic(HTML == '&emsp;indented')) :-
		issue_creator<<description_format(azure, '&emsp;indented', HTML).

	test(issue_creator_format_jira_empty, deterministic(Description == '')) :-
		issue_creator<<description_format(jira, '', Description).

	test(issue_creator_format_jira_indentation, deterministic(Description == 'first\nsecondthird')) :-
		issue_creator<<description_format(jira, '&emsp;first\n&emsp;&emsp;second&emsp;third&emsp;', Description).

	test(issue_creator_format_jira_preserve, deterministic(Description == '`code` <tag> &amp; &ems &emsp')) :-
		issue_creator<<description_format(jira, '`code` <tag> &amp; &ems &emsp', Description).

	test(issue_creator_format_github, deterministic(Description == '&emsp;`code`\n<tag>')) :-
		issue_creator<<description_format(github, '&emsp;`code`\n<tag>', Description).

	test(issue_creator_format_gitlab, deterministic(Description == '&emsp;`code`\n<tag>')) :-
		issue_creator<<description_format(gitlab, '&emsp;`code`\n<tag>', Description).

	test(issue_creator_format_gitea, deterministic(Description == '&emsp;`code`\n<tag>')) :-
		issue_creator<<description_format(gitea, '&emsp;`code`\n<tag>', Description).

	test(issue_creator_format_linear, deterministic(Description == '&emsp;`code`\n<tag>')) :-
		issue_creator<<description_format(linear, '&emsp;`code`\n<tag>', Description).

	test(issue_creator_wiql_single_label, deterministic(Query == 'SELECT [System.Id] FROM WorkItems WHERE [System.WorkItemType] = \'Bug\' AND [System.Title] = \'sample\' AND [System.Tags] CONTAINS \'bug\'')) :-
		issue_creator<<azure_wiql(sample, bug, Query).

	test(issue_creator_wiql_multiple_labels, deterministic(Query == 'SELECT [System.Id] FROM WorkItems WHERE [System.WorkItemType] = \'Bug\' AND [System.Title] = \'sample\' AND [System.Tags] CONTAINS \'bug\' AND [System.Tags] CONTAINS \'auto\'')) :-
		issue_creator<<azure_wiql(sample, 'bug,auto', Query).

	test(issue_creator_wiql_apostrophes, deterministic(Query == 'SELECT [System.Id] FROM WorkItems WHERE [System.WorkItemType] = \'Bug\' AND [System.Title] = \'can\'\'t\' AND [System.Tags] CONTAINS \'team-s\'')) :-
		issue_creator<<azure_wiql('can\'t', 'team-s', Query).

	test(issue_creator_azure_no_matches, true) :-
		issue_creator<<azure_search_command(unix, 'printf Project', 'printf \'"New","Investigating"\'', 'printf 0', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_existing, false) :-
		issue_creator<<azure_search_command(unix, 'printf Project', 'printf \'"New"\'', 'printf 1', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_project_error, false) :-
		issue_creator<<azure_search_command(unix, false, 'printf \'"New"\'', 'printf 0', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_states_error, false) :-
		issue_creator<<azure_search_command(unix, 'printf Project', false, 'printf 0', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_query_error, false) :-
		issue_creator<<azure_search_command(unix, 'printf Project', 'printf \'"New"\'', false, '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_empty_states, false) :-
		issue_creator<<azure_search_command(unix, 'printf Project', true, 'printf 0', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_literal_query, true) :-
		issue_creator<<command_text(unix, printf, ['%s', 'Project $HOME'], ProjectQuery),
		issue_creator<<command_text(unix, sh, ['-c', 'test "$#" -eq 2 && test "$2" = \'project=Project $HOME\' && printf "%s" "$1"', sh, '"New","Team\'s review"'], StatesQuery),
		issue_creator<<command_text(unix, sh, ['-c', 'test "$#" -eq 2 && test "$1" = "$2" && printf 0', sh, 'SELECT $HOME AND [System.TeamProject] = "Project $HOME" AND [System.State] IN ("New","Team\'s review")'], IssueQuery),
		issue_creator<<azure_search_command(unix, ProjectQuery, StatesQuery, IssueQuery, '\'SELECT $HOME\'', Command),
		os::shell(Command).

	test(issue_creator_azure_utf16le, deterministic(Bytes == [65, 0, 172, 32, 61, 216, 0, 222]), [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		atom_codes(Program, [65, 8364, 128512]),
		issue_creator<<powershell_command(Program, Command),
		atom_concat('powershell -NoLogo -NoProfile -NonInteractive -EncodedCommand ', Encoded, Command),
		base64::parse(atom(Encoded), Bytes).

	test(issue_creator_azure_command_unix, true) :-
		issue_creator<<azure_list_command(unix, sample, bug, Command),
		assertion(sub_atom(Command, _, _, _, '\'az\' \'devops\' \'project\' \'show\'')),
		assertion(sub_atom(Command, _, _, _, '\'--resource\' \'workItemTypeStates\' \'--api-version\' \'7.1\'')),
		assertion(sub_atom(Command, _, _, _, '\'--route-parameters\' \'type=Bug\' "project=$Project"')),
		assertion(sub_atom(Command, _, _, _, '[System.TeamProject]')),
		assertion(sub_atom(Command, _, _, _, '[System.State] IN ($States)')),
		assertion(\+ sub_atom(Command, _, _, _, 'System.StateCategory')).

	test(issue_creator_azure_command_windows, true) :-
		issue_creator<<azure_list_command(windows, 'sample %PATH% !value! $HOME', bug, Command),
		atom_concat('powershell -NoLogo -NoProfile -NonInteractive -EncodedCommand ', Encoded, Command),
		base64::parse(atom(Encoded), Bytes),
		utf_16le::bytes_to_codes(Bytes, Codes),
		atom_codes(Program, Codes),
		assertion(sub_atom(Program, _, _, _, '\'--resource\' \'workItemTypeStates\'')),
		assertion(sub_atom(Program, _, _, _, '(\'project=\' + $Project)')),
		assertion(sub_atom(Program, _, _, _, 'sample %PATH% !value! $HOME')),
		assertion(sub_atom(Program, _, _, _, '$LASTEXITCODE -eq 0')),
		assertion(\+ sub_atom(Program, _, _, _, 'System.StateCategory')).

	test(issue_creator_azure_empty_project, false) :-
		issue_creator<<azure_search_command(unix, true, 'printf \'"New"\'', 'printf 0', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_azure_query_error_with_zero, false) :-
		issue_creator<<azure_search_command(unix, 'printf Project', 'printf \'"New"\'', 'sh -c \'printf 0; exit 1\' sh', '\'SELECT\'', Command),
		os::shell(Command).

	test(issue_creator_jql, deterministic(Query == 'issuetype = Bug AND statusCategory != Done AND labels = "bug" AND labels = "auto" AND summary ~ "\\"sample\\""')) :-
		issue_creator<<jira_jql(sample, 'bug,auto', Query).

	test(issue_creator_jql_unix, true) :-
		issue_creator<<jira_list_command(unix, sample, bug, Command),
		assertion(sub_atom(Command, _, _, _, '\'--paginate\' \'1\'')),
		assertion(sub_atom(Command, _, _, _, '\'-q\' \'issuetype = Bug')).

	test(issue_creator_jql_windows, true) :-
		issue_creator<<jira_list_command(windows, sample, bug, Command),
		assertion(sub_atom(Command, 0, _, _, '"jira" "issue" "list"')),
		assertion(sub_atom(Command, _, _, _, '"--paginate" "1" "-q" "issuetype = Bug AND statusCategory != Done')),
		assertion(sub_atom(Command, _, _, _, ' AND labels = \\"bug\\"')),
		atom_chars(Phrase, ['\\', '"', '\\', '\\', '\\', '"', s, a, m, p, l, e, '\\', '\\', '\\', '"', '\\', '"', '"']),
		assertion(sub_atom(Command, _, _, 0, Phrase)).

	test(issue_creator_jql_escaped_title, true) :-
		issue_creator<<jira_jql('a"b\\c', bug, Query),
		atom_chars(Phrase, ['"', '\\', '"', a, '\\', '\\', '\\', '"', b, '\\', '\\', '\\', '\\', c, '\\', '"', '"']),
		assertion(sub_atom(Query, _, _, 0, Phrase)).

	test(issue_creator_jql_label, true) :-
		issue_creator<<jira_jql(sample, 'team-label_2', Query),
		assertion(sub_atom(Query, _, _, _, ' AND labels = "team-label_2"')).

	test(issue_creator_labels_valid, deterministic) :-
		forall(
			list::member(Labels, [bug, 'bug,auto,test-failures', 'A_Z,0-9']),
			issue_creator<<valid_labels(Labels)
		).

	test(issue_creator_labels_invalid, deterministic) :-
		forall(
			list::member(Labels, ['', ',bug', 'bug,', 'bug,,auto', 'test failures', 'team\'s', 'team"s', 'team\\s', '$HOME', '%PATH%', 'bug;auto', 'bug&auto', '-bug', '_bug', 'bug\nother']),
			\+ issue_creator<<valid_labels(Labels)
		).

	test(issue_creator_labels_invalid_blocks_reporting, false) :-
		with_flag(issue_labels, 'test failures', issue_creator<<create_bug_report(suite, sample, _, 1-1, failure_instead_of_success, '', 0)).

	test(issue_creator_jql_apostrophe, true) :-
		issue_creator<<jira_list_command(unix, 'can\'t', bug, Command),
		atom_chars(Fragment, [c, a, n, '\'', '\\', '\'', '\'', t]),
		assertion(sub_atom(Command, _, _, _, Fragment)).

	test(issue_creator_shell_quote_unix_apostrophe, deterministic(Chars == ['\'', a, '\'', '\\', '\'', '\'', b, '\''])) :-
		issue_creator<<shell_quote(unix, 'a\'b', Quoted),
		atom_chars(Quoted, Chars).

	test(issue_creator_shell_quote_windows, deterministic(Quoted == '"a\\"b\\\\c"')) :-
		issue_creator<<shell_quote(windows, 'a"b\\c', Quoted).

	test(issue_creator_backslash_escape_empty, deterministic(Escaped == '')) :-
		issue_creator<<backslash_escape('', Escaped).

	test(issue_creator_backslash_escape_quotes, deterministic(Escaped == 'a\\"b\\\\c')) :-
		issue_creator<<backslash_escape('a"b\\c', Escaped).

	test(issue_creator_tags_empty, deterministic(Tags == '')) :-
		issue_creator<<azure_labels_to_tags('', Tags).

	test(issue_creator_tags_single, deterministic(Tags == bug)) :-
		issue_creator<<azure_labels_to_tags(bug, Tags).

	test(issue_creator_tags_multiple, deterministic(Tags == 'bug;auto;test-failures')) :-
		issue_creator<<azure_labels_to_tags('bug,auto,test-failures', Tags).

	test(issue_creator_escape_empty, deterministic(Escaped == '')) :-
		issue_creator<<escape_double_quotes('', Escaped).

	test(issue_creator_url_github, deterministic(URL == 'https://example.com/repo/tests.lgt#L10')) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<tests_url(github, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_gitlab, deterministic(URL == 'https://example.com/repo/tests.lgt#L10')) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<tests_url(gitlab, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_gitea, deterministic(URL == 'https://example.com/repo/tests.lgt#L10')) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<tests_url(gitea, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_jira, deterministic(URL == 'https://example.com/repo/tests.lgt#L10')) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<tests_url(jira, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_linear, deterministic(URL == 'https://example.com/repo/tests.lgt#L10')) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<tests_url(linear, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_azure, deterministic(URL == 'https://dev.azure.com/org/project/_git/repo?path=/tests.lgt&line=10')) :-
		with_flag(
			tests_base_url,
			'https://dev.azure.com/org/project/_git/repo',
			issue_creator<<tests_url(azure, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_azure_absolute_path, deterministic(URL == 'https://dev.azure.com/org/project/_git/repo?path=/tests.lgt&line=10')) :-
		with_flag(
			tests_base_url,
			'https://dev.azure.com/org/project/_git/repo',
			issue_creator<<tests_url(azure, '/tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_azure_version, deterministic(URL == 'https://dev.azure.com/org/project/_git/repo?version=GCabc123&path=/tests.lgt&line=10')) :-
		with_flag(
			tests_base_url,
			'https://dev.azure.com/org/project/_git/repo?version=GCabc123',
			issue_creator<<tests_url(azure, 'tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_azure_version_absolute_path, deterministic(URL == 'https://dev.azure.com/org/project/_git/repo?version=GCabc123&path=/tests.lgt&line=10')) :-
		with_flag(
			tests_base_url,
			'https://dev.azure.com/org/project/_git/repo?version=GCabc123',
			issue_creator<<tests_url(azure, '/tests.lgt', 10-12, URL)
		).

	test(issue_creator_url_empty, false) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<tests_url(github, 'tests.lgt', 10-12, _)
		).

	test(issue_creator_url_azure_empty, false) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<tests_url(azure, 'tests.lgt', 10-12, _)
		).

	test(issue_creator_url_invalid_position, false) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<tests_url(github, 'tests.lgt', unknown, _)
		).

	test(issue_creator_url_azure_invalid_position, false) :-
		with_flag(
			tests_base_url,
			'https://dev.azure.com/org/project/_git/repo',
			issue_creator<<tests_url(azure, 'tests.lgt', unknown, _)
		).

	test(issue_creator_description_url_fallback, true) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<description(github, suite, 'tests.lgt', 10-12, failure_instead_of_success, '', 1, main, abc123, author, date, message, Description)
		),
		assertion(sub_atom(Description, _, _, _, 'Test file: tests.lgt:10-12\n\n')).

	test(issue_creator_description_position_fallback, true) :-
		with_flag(tests_base_url, 'https://example.com/repo/',
			issue_creator<<description(github, suite, 'tests.lgt', unknown, failure_instead_of_success, '', 1, main, abc123, author, date, message, Description)),
		assertion(sub_atom(Description, _, _, _, 'Test file: tests.lgt:unknown\n\n')).

	test(issue_creator_title_atom_test_name, deterministic(Title == 'Test sample of test set suite/ failed')) :-
		issue_creator<<title(sample, 'suite/', Title).

	test(issue_creator_title_compound_test_name, deterministic(Title == 'Test sample(a) of test set suite/ failed')) :-
		issue_creator<<title(sample(a), 'suite/', Title).

	test(issue_creator_description_complete_linked, deterministic(Description == 'Test object: `suite(a)`  \nTest file: https://example.com/repo/tests.lgt#L10\n\nFailure:  \n&emsp;test goal failed but should have succeeded\n\nNote: (none)\n\nTime: 1 seconds\n\nGit branch: main  \nCommit hash: abc123  \nCommit author: A. Author  \nCommit date: 2026-10-26  \nCommit message:  \n&emsp;Fix sample\n')) :-
		with_flag(
			tests_base_url,
			'https://example.com/repo/',
			issue_creator<<description(github, suite(a), 'tests.lgt', 10-12, failure_instead_of_success, '', 1, main, abc123, 'A. Author', '2026-10-26', 'Fix sample', Description)
		).

	test(issue_creator_description_complete_fallback, deterministic(Description == 'Test object: `suite(A)`  \nTest file: tests.lgt:10-12\n\nFailure:  \nunknown(detail)\n\nNote: note(detail)\n\nTime: 1 seconds\n\nGit branch: main  \nCommit hash: abc123  \nCommit author: A. Author  \nCommit date: 2026-10-26  \nCommit message:  \n&emsp;Fix sample\n')) :-
		with_flag(
			tests_base_url,
			'',
			issue_creator<<description(github, suite(_), 'tests.lgt', 10-12, unknown(detail), note(detail), 1, main, abc123, 'A. Author', '2026-10-26', 'Fix sample', Description)
		).

	test(issue_creator_description_azure_formatted, true) :-
		with_flag(
			tests_base_url,
			'https://dev.azure.com/org/project/_git/repo',
			issue_creator<<description(azure, suite, 'tests.lgt', 10-12, failure_instead_of_success, 'needs review', 1, main, abc123, author, date, message, Markdown)
		),
		issue_creator<<description_format(azure, Markdown, HTML),
		assertion(sub_atom(HTML, 0, _, _, 'Test object: <code>suite</code>  <br>')),
		assertion(sub_atom(HTML, _, _, _, 'Test file: https://dev.azure.com/org/project/_git/repo?path=/tests.lgt&line=10<br><br>')),
		assertion(sub_atom(HTML, _, _, _, 'Note: needs review<br><br>')).

	test(issue_creator_flaky_exact, true) :-
		issue_creator<<flaky_test_failure(flaky).

	test(issue_creator_flaky_embedded, true) :-
		issue_creator<<flaky_test_failure('known flaky test').

	test(issue_creator_flaky_empty, false) :-
		issue_creator<<flaky_test_failure('').

	test(issue_creator_flaky_unrelated, false) :-
		issue_creator<<flaky_test_failure('needs review').

	test(issue_creator_flaky_case_sensitive, false) :-
		issue_creator<<flaky_test_failure('Flaky').

	test(issue_creator_flaky_number, false) :-
		issue_creator<<flaky_test_failure(42).

	test(issue_creator_flaky_compound, false) :-
		issue_creator<<flaky_test_failure(note(flaky)).

	test(issue_creator_flaky_variable, false) :-
		issue_creator<<flaky_test_failure(_).

	test(issue_creator_server_default, deterministic(Server == github)) :-
		issue_creator<<server('', Server).

	test(issue_creator_server_github, deterministic(Server == github)) :-
		issue_creator<<server(github, Server).

	test(issue_creator_server_gitlab, deterministic(Server == gitlab)) :-
		issue_creator<<server(gitlab, Server).

	test(issue_creator_server_azure, deterministic(Server == azure)) :-
		issue_creator<<server(azure, Server).

	test(issue_creator_server_gitea, deterministic(Server == gitea)) :-
		issue_creator<<server(gitea, Server).

	test(issue_creator_server_jira, deterministic(Server == jira)) :-
		issue_creator<<server(jira, Server).

	test(issue_creator_server_linear, deterministic(Server == linear)) :-
		issue_creator<<server(linear, Server).

	test(issue_creator_server_unsupported, false) :-
		issue_creator<<server(unsupported, _).

	test(issue_creator_server_flag_default, deterministic(Server == github)) :-
		with_flag(issue_server, '', issue_creator<<issue_server(Server)).

	test(issue_creator_server_flag_jira, deterministic(Server == jira)) :-
		with_flag(issue_server, jira, issue_creator<<issue_server(Server)).

	test(issue_creator_server_flag_linear, deterministic(Server == linear)) :-
		with_flag(issue_server, linear, issue_creator<<issue_server(Server)).

	test(issue_creator_server_flag_unsupported, false) :-
		with_flag(issue_server, unsupported, issue_creator<<issue_server(_)).

	test(issue_creator_flag_restored_after_success, true) :-
		current_logtalk_flag(tests_base_url, Original),
		with_flag(tests_base_url, 'https://example.com/fixture/', current_logtalk_flag(tests_base_url, 'https://example.com/fixture/')),
		current_logtalk_flag(tests_base_url, Restored),
		assertion(Restored == Original).

	test(issue_creator_flag_restored_after_failure, true) :-
		current_logtalk_flag(issue_server, Original),
		\+ with_flag(issue_server, unsupported, fail),
		current_logtalk_flag(issue_server, Restored),
		assertion(Restored == Original).

	test(issue_creator_flag_restored_after_exception, true) :-
		current_logtalk_flag(issue_server, Original),
		catch(with_flag(issue_server, unsupported, throw(flag_test_error)), Error, true),
		current_logtalk_flag(issue_server, Restored),
		assertion(Error == flag_test_error),
		assertion(Restored == Original).

	test(issue_creator_run_cli_literal_arguments, true) :-
		issue_creator<<run_cli(unix, sh, ['-c', 'test "$#" -eq 1 && test "$1" = "$(printf "a\\047b \\042\\044HOME\\042")"', sh, 'a\'b "$HOME"']).

	test(issue_creator_run_cli_failure, false) :-
		issue_creator<<run_cli(unix, sh, ['-c', 'printf partial; exit 1']).

	test(issue_creator_run_cli_empty_success, true) :-
		issue_creator<<run_cli(unix, printf, ['%s', '']).

	test(issue_creator_run_cli_missing_executable, false) :-
		issue_creator<<run_cli(unix, logtalk_issue_creator_nonexistent_executable, []).

	test(issue_creator_create_arguments_literal_labels, deterministic(Arguments == [issue, create, '--title', 'can\'t', '--description', 'body\nnext', '--yes', '--no-input', '-l', bug, '-l', 'test-failures', '--assignee', 'author@example.com'])) :-
		issue_creator<<create_arguments(linear, 'can\'t', 'body\nnext', 'bug,test-failures', 'author@example.com', linear, Arguments).

	test(issue_creator_create_arguments_github_unassigned, deterministic(Arguments == [issue, create, '--title', title, '--body', body, '--label', 'bug,test-failures'])) :-
		issue_creator<<create_arguments(github, title, body, 'bug,test-failures', '', gh, Arguments).

	test(issue_creator_create_arguments_azure_fields, deterministic(Arguments == [boards, 'work-item', create, '--type', 'Bug', '--title', title, '--fields', 'Microsoft.VSTS.TCM.ReproSteps=<code>body</code>', 'System.Tags=bug;test-failures', '--output', none])) :-
		issue_creator<<create_arguments(azure, title, '<code>body</code>', 'bug,test-failures', '', az, Arguments).

	test(issue_creator_create_arguments_jira_assigned, deterministic(Arguments == [issue, create, '-tBug', '-s', title, '-b', body, '--no-input', '-l', bug, '-l', 'test-failures', '-a', 'author@example.com'])) :-
		issue_creator<<create_arguments(jira, title, body, 'bug,test-failures', 'author@example.com', jira, Arguments).

	test(issue_creator_create_arguments_jira_unassigned, deterministic(Arguments == [issue, create, '-tBug', '-s', title, '-b', body, '--no-input', '-l', bug])) :-
		issue_creator<<create_arguments(jira, title, body, bug, '', jira, Arguments).

	test(issue_creator_create_arguments_gitea_assigned, deterministic(Arguments == [issues, create, '--title', title, '--description', body, '--labels', 'bug,test-failures', '--assignees', author])) :-
		issue_creator<<create_arguments(gitea, title, body, 'bug,test-failures', author, tea, Arguments).

	test(issue_creator_create_arguments_gitea_unassigned, deterministic(Arguments == [issues, create, '--title', title, '--description', body, '--labels', bug])) :-
		issue_creator<<create_arguments(gitea, title, body, bug, '', tea, Arguments).

	test(issue_creator_search_github, deterministic(Arguments == [issue, list, '--state', open, '--label', 'bug,test-failures', '--search', '"title" in:title', '--limit', '1', '--json', number, '--jq', length])) :-
		issue_creator<<search_arguments(github, title, 'bug,test-failures', gh, Arguments).

	test(issue_creator_search_gitlab, deterministic(Arguments == [issue, list, '--label', bug, '--search', title, '--in', title, '--per-page', '1', '--output', json, '--jq', length])) :-
		issue_creator<<search_arguments(gitlab, title, bug, glab, Arguments).

	test(issue_creator_search_gitea, deterministic(Arguments == [issues, list, '--state', open, '--labels', bug, '--keyword', title, '--fields', index, '--output', simple, '--limit', '1'])) :-
		issue_creator<<search_arguments(gitea, title, bug, tea, Arguments).

	test(issue_creator_search_linear, deterministic(Arguments == [issue, query, '--search', title, '--state', triage, '--state', backlog, '--state', unstarted, '--state', started, '--limit', '1', '--json', '--no-pager', '--no-input', '-l', bug, '-l', 'test-failures'])) :-
		issue_creator<<search_arguments(linear, title, 'bug,test-failures', linear, Arguments).

	test(issue_creator_search_count_absent, true) :-
		issue_creator<<search_command(unix, github, 'printf "0\\n"', Command),
		os::shell(Command).

	test(issue_creator_search_count_existing, false) :-
		issue_creator<<search_command(unix, gitlab, 'printf "1\\n"', Command),
		os::shell(Command).

	test(issue_creator_search_gitea_absent, true) :-
		issue_creator<<search_command(unix, gitea, 'printf ""', Command),
		os::shell(Command).

	test(issue_creator_search_gitea_existing, false) :-
		issue_creator<<search_command(unix, gitea, 'printf "123\\n"', Command),
		os::shell(Command).

	test(issue_creator_search_linear_absent, true) :-
		issue_creator<<search_command(unix, linear, 'printf "[]\\n"', Command),
		os::shell(Command).

	test(issue_creator_search_linear_existing, false) :-
		issue_creator<<search_command(unix, linear, 'printf "[{issue}]\\n"', Command),
		os::shell(Command).

	test(issue_creator_search_failure_closed, false) :-
		issue_creator<<search_command(unix, github, '(printf "0\\n"; exit 1)', Command),
		os::shell(Command).

	test(issue_creator_search_empty_output_closed, false) :-
		issue_creator<<search_command(unix, github, 'printf ""', Command),
		os::shell(Command).

	test(issue_creator_search_gitea_failure_closed, false) :-
		issue_creator<<search_command(unix, gitea, 'false', Command),
		os::shell(Command).

	test(issue_creator_search_linear_failure_closed, false) :-
		issue_creator<<search_command(unix, linear, '(printf "[]\\n"; exit 1)', Command),
		os::shell(Command).

	test(issue_creator_search_windows_count_command, deterministic(Command == '(query 2>nul && echo __logtalk_query_ok__) | findstr /n "^" | findstr /l /x /c:"1:0" /c:"2:__logtalk_query_ok__" | find /c /v "" | findstr /x /c:"2" >nul')) :-
		issue_creator<<search_command(windows, github, query, Command).

	test(issue_creator_search_windows_empty_command, deterministic(Command == '(query 2>nul && echo __logtalk_query_ok__) | findstr /n "^" | findstr /l /x /c:"1:__logtalk_query_ok__" >nul')) :-
		issue_creator<<search_command(windows, gitea, query, Command).

	test(issue_creator_title_legacy_format, deterministic(Title == 'Test invoice_creation of test set library/invoices failed')) :-
		issue_creator<<title(invoice_creation, 'library/invoices', Title).

	test(issue_creator_jira_no_result, true) :-
		issue_creator<<command_text(unix, sh, ['-c', 'printf "%s\\n" "$1" >&2; exit 1', sh, 'No result found for given query in project "TEST"'], Query),
		issue_creator<<jira_search_command(unix, Query, Command),
		os::shell(Command).

	test(issue_creator_jira_authentication_error, false) :-
		issue_creator<<command_text(unix, sh, ['-c', 'printf "%s\\n" "Error: authentication failed" >&2; exit 1'], Query),
		issue_creator<<jira_search_command(unix, Query, Command),
		os::shell(Command).

	test(issue_creator_jira_existing, false) :-
		issue_creator<<jira_search_command(unix, 'printf "TEST-1\\n"', Command),
		os::shell(Command).

	test(issue_creator_jira_silent_failure, false) :-
		issue_creator<<jira_search_command(unix, 'false', Command),
		os::shell(Command).

	test(issue_creator_jira_missing_executable, false) :-
		issue_creator<<jira_search_command(unix, logtalk_issue_creator_nonexistent_executable, Command),
		os::shell(Command).

	test(issue_creator_title_absolute_directory, deterministic(Title == 'Test sample of test set /project/suite/ failed')) :-
		issue_creator<<title(sample, '/project/suite/', Title).

	test(issue_creator_title_distinct_directories, deterministic(First \== Second)) :-
		issue_creator<<title(sample, 'suite/first/', First),
		issue_creator<<title(sample, 'suite/second/', Second).

	test(issue_creator_title_empty_directory, deterministic(Title == 'Test sample of test set  failed')) :-
		issue_creator<<title(sample, '', Title).

	% auxiliary predicates


	:- meta_predicate(with_flag(*, *, 0)).

	with_flag(Flag, Value, Goal) :-
		current_logtalk_flag(Flag, Original),
		set_logtalk_flag(Flag, Value),
		catch(
			(	lgtunit::deterministic(Goal, Deterministic) ->
				Result = success(Deterministic)
			;	Result = failure
			),
			Error,
			(	set_logtalk_flag(Flag, Original),
				throw(Error)
			)
		),
		set_logtalk_flag(Flag, Original),
		Result = success(Deterministic),
		assertion(Deterministic == true).

:- end_object.
