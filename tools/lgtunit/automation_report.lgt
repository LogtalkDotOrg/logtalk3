%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
	create_logtalk_flag(test_results_directory, './', [type(atom), keep(true)]),
	create_logtalk_flag(test_unit_name, '', [type(atom), keep(true)])
)).


:- object(automation_report).

	:- info([
		version is 1:1:1,
		author is 'Paulo Moura',
		date is 2020-10-21,
		comment is 'Intercepts unit test execution messages and generates a ``*.totals`` files for parsing by the ``logtalk_tester.sh`` automation shell script.',
		remarks is [
			'Usage' - 'Automatically loaded by the ``logtalk_tester.sh`` shell script.'
		]
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message),
		% allow default processing of the messages
		fail.

	% start
	message_hook(tests_started) :-
		{current_logtalk_flag(test_results_directory, Directory)},
		{current_logtalk_flag(test_unit_name, Name)},
		atom_concat('/', Name, ResultsFile0),
		atom_concat(ResultsFile0, '.totals', ResultsFile1),
		atom_concat(Directory, ResultsFile1, ResultsFile),
		open(ResultsFile, write, _, [alias(results_file)]).
	message_hook(running_tests_from_object_file(_, File)) :-
		write(results_file, 'file\t'), write(results_file, File), nl(results_file).
	% test results summary
	message_hook(tests_results_summary(Object, Total, Skipped, Passed, Failed, _)) :-
		write(results_file, 'object\t'), writeq(results_file, Object),
		write(results_file, '\t'), write(results_file, Total),
		write(results_file, '\t'), write(results_file, Skipped),
		write(results_file, '\t'), write(results_file, Passed),
		write(results_file, '\t'), write(results_file, Failed), nl(results_file).
	% failed tests
	message_hook(failed_test(Object, Test, _, _, _, _)) :-
		write(results_file, 'failed\t'), writeq(results_file, Test), write(results_file, ' @ '), writeq(results_file, Object), nl(results_file).
	message_hook(non_deterministic_success(Object, Test, _, _, _)) :-
		write(results_file, 'failed\t'), writeq(results_file, Test), write(results_file, ' @ '), writeq(results_file, Object), nl(results_file).
	% skipped test
	message_hook(skipped_test(Object, Test, _, _, _)) :-
		write(results_file, 'skipped\t'), writeq(results_file, Test), write(results_file, ' @ '), writeq(results_file, Object), nl(results_file).
	% code coverage results
	message_hook(covered_clause_numbers(_, _, Percentage)) :-
		write(results_file, 'coverage\t'), write(results_file, Percentage), write(results_file, '%'), nl(results_file).
	message_hook(no_code_coverage_information_collected) :-
		write(results_file, 'coverage\tn/a'), nl(results_file).
	message_hook(tests_ended) :-
		close(results_file).

:- end_object.
