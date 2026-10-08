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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2026-10-08,
		comment is 'Unit tests for the "git" library.'
	]).

	:- uses(git, [
		branch/2, commit_log/3,
		commit_author/2, commit_date/2, commit_message/2,
		commit_hash/2, commit_hash_abbreviated/2, working_tree_clean/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(git).

	:- if(os::operating_system_type(windows)).

		condition :-
			os::shell('git --version >nul 2>&1'),
			os::shell('tar --version >nul 2>&1').

		setup :-
			test_repo(Repo, Directory),
			atomic_list_concat(['tar -xf "', Repo, '.zip"', ' -C "', Directory, '"'], Command),
			os::shell(Command).

		cleanup :-
			test_repo(Repo, _),
			atomic_list_concat(['rmdir /s /q "', Repo, '"'], Command),
			os::shell(Command).

	:- else.

		condition :-
			os::shell('git --version > /dev/null 2>&1'),
			os::shell('unzip -v > /dev/null 2>&1').

		setup :-
			test_repo(Repo, Directory),
			atomic_list_concat(['unzip "', Repo, '.zip"', ' -d "', Directory, '"'], Command),
			os::shell(Command).

		cleanup :-
			test_repo(Repo, _),
			atomic_list_concat(['rm -rf "', Repo, '"'], Command),
			os::shell(Command).

	:- endif.

	% when the directory is not a git repo, the predicates
	% are expected to fail

	test(git_branch_2_01, false) :-
		branch('/', _).

	test(git_branch_2_02, true(Branch == master)) :-
		test_repo(Repo, _),
		branch(Repo, Branch).

	test(git_commit_log_3_01, false) :-
		commit_log('/', '%h', _).

	test(git_commit_log_3_02, true) :-
		test_repo(Repo, _),
		commit_log(Repo, '%h', _).

	test(git_commit_author_2_01, false) :-
		commit_author('/', _).

	test(git_commit_author_2_02, true(Author == 'John Doe')) :-
		test_repo(Repo, _),
		commit_author(Repo, Author).

	test(git_commit_date_2_01, false) :-
		commit_date('/', _).

	test(git_commit_message_2_01, false) :-
		commit_message('/', _).

	test(git_commit_message_2_02, true(Message == 'First commit\n')) :-
		test_repo(Repo, _),
		commit_message(Repo, Message).

	test(git_commit_hash_2_01, false) :-
		commit_hash('/', _).

	test(git_commit_hash_2_02, true(Hash == '02a812ed805949d3aaf15240254c27564eff35c5')) :-
		test_repo(Repo, _),
		commit_hash(Repo, Hash).

	test(git_commit_hash_abbreviated_2_01, false) :-
		commit_hash_abbreviated('/', _).

	test(git_commit_hash_abbreviated_2_02, true(Hash == '02a812e')) :-
		test_repo(Repo, _),
		commit_hash_abbreviated(Repo, Hash).

	test(git_working_tree_clean_1_01, false) :-
		working_tree_clean('/').

	test(git_working_tree_clean_1_02, deterministic) :-
		test_repo(Repo, _),
		working_tree_clean(Repo).

	test(git_working_tree_clean_1_03, deterministic) :-
		test_repo(Repo, _),
		os::path_concat(Repo, 'untracked.txt', File),
		write_dirty_fixture(File),
		^^assertion(git::working_tree_clean(Repo)),
		os::delete_file(File),
		working_tree_clean(Repo).

	test(git_working_tree_clean_1_04, deterministic(Hash == '02a812ed805949d3aaf15240254c27564eff35c5')) :-
		test_repo(Repo, _),
		os::path_concat(Repo, 'a.lgt', File),
		write_dirty_fixture(File),
		^^assertion(\+ git::working_tree_clean(Repo)),
		commit_hash(Repo, Hash),
		fixture_git_command(Repo, 'checkout -- a.lgt'),
		working_tree_clean(Repo).

	test(git_working_tree_clean_1_05, deterministic) :-
		test_repo(Repo, _),
		os::path_concat(Repo, 'a.lgt', File),
		write_dirty_fixture(File),
		fixture_git_command(Repo, 'add a.lgt'),
		^^assertion(\+ git::working_tree_clean(Repo)),
		fixture_git_command(Repo, 'reset -q HEAD -- a.lgt'),
		fixture_git_command(Repo, 'checkout -- a.lgt'),
		working_tree_clean(Repo).

	test(git_working_tree_clean_1_06, deterministic) :-
		test_repo(Repo, _),
		os::path_concat(Repo, 'a.lgt', File),
		os::delete_file(File),
		^^assertion(\+ git::working_tree_clean(Repo)),
		fixture_git_command(Repo, 'checkout -- a.lgt'),
		working_tree_clean(Repo).

	% auxiliary predicates

	write_dirty_fixture(File) :-
		open(File, write, Stream),
		write(Stream, dirty),
		close(Stream).

	fixture_git_command(Repo, Arguments) :-
		atomic_list_concat(['git -C "', Repo, '" ', Arguments], Command),
		os::shell(Command).

	test_repo(Repo, Directory) :-
		this(This),
		object_property(This, file(_, Directory0)),
		os::path_concat(Directory0, repo, Repo0),
		os::internal_os_path(Repo0, Repo),
		os::internal_os_path(Directory0, Directory).

:- end_object.
