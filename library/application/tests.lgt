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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-03-24,
		comment is 'Unit tests for the "application" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(os, [
		path_concat/3
	]).

	cover(application_common).

	setup :-
		create_object(application_fixture, [imports(application_common)], [], [
			name(test_application),
			version('1.2.3'),
			description('Application metadata fixture'),
			license('Apache-2.0'),
			homepage('https://example.com/test_application'),
			distribution('https://example.com/test_application/releases/download/v1.2.3/test_application.tgz'),
			loader_file('/path/to/test_application/loader.lgt'),
			creators(['Tool: Build pipeline', 'Person: Alice Example']),
			supplier('Organization: Example Application'),
			originator('Person: Alice Example'),
			built_date('2026-03-24T00:00:00Z'),
			release_date('2026-03-24T00:00:00Z'),
			valid_until_date('2027-03-24T00:00:00Z'),
			repository('https://example.com/example-application.git'),
			repository_branch(master),
			repository_commit('02a812ed805949d3aaf15240254c27564eff35c5'),
			repository_commit_abbreviated('02a812e'),
			repository_commit_date('2020-11-12T16:02:00+00:00'),
			repository_commit_author('John Doe'),
			repository_commit_message('First commit\n')
		]),
		create_object(sparse_application, [imports(application_common)], [], [
			name(sparse_application),
			creators(['Tool: Build pipeline'])
		]).

	cleanup :-
		(   current_object(application_fixture) ->
			abolish_object(application_fixture)
		;   true
		),
		(   current_object(sparse_application) ->
			abolish_object(sparse_application)
		;   true
		).

	fixture_object(application_fixture).

	sparse_object(sparse_application).

	test(application_name_1_01, true(Name == test_application)) :-
		fixture_object(Object),
		Object::name(Name).

	test(application_creators_1_01, true(Creators == ['Tool: Build pipeline', 'Person: Alice Example'])) :-
		fixture_object(Object),
		Object::creators(Creators).

	test(application_loader_file_1_01, true(Loader == '/path/to/test_application/loader.lgt')) :-
		fixture_object(Object),
		Object::loader_file(Loader).

	test(application_loader_file_1_02, true(Loader == ExpectedLoader)) :-
		this(This),
		object_property(This, file(_, Directory)),
		path_concat(Directory, 'loader.lgt', ExpectedLoader),
		implicit_loader_metadata::loader_file(Loader).

	test(application_external_reference_2_01, true) :-
		fixture_object(Object),
		findall(Type-URL, Object::external_reference(Type, URL), References),
		memberchk(homepage-'https://example.com/test_application', References),
		memberchk(distribution-'https://example.com/test_application/releases/download/v1.2.3/test_application.tgz', References),
		memberchk(repository-'https://example.com/example-application.git', References).

	test(application_repository_1_01, true(URL == 'https://example.com/example-application.git')) :-
		fixture_object(Object),
		Object::repository(URL).

	test(application_repository_branch_1_01, true(Branch == master)) :-
		fixture_object(Object),
		Object::repository_branch(Branch).

	test(application_repository_commit_1_01, true(Hash == '02a812ed805949d3aaf15240254c27564eff35c5')) :-
		fixture_object(Object),
		Object::repository_commit(Hash).

	test(application_repository_commit_abbreviated_1_01, true(Hash == '02a812e')) :-
		fixture_object(Object),
		Object::repository_commit_abbreviated(Hash).

	test(application_repository_commit_date_1_01, true(Date == '2020-11-12T16:02:00+00:00')) :-
		fixture_object(Object),
		Object::repository_commit_date(Date).

	test(application_repository_commit_author_1_01, true(Author == 'John Doe')) :-
		fixture_object(Object),
		Object::repository_commit_author(Author).

	test(application_repository_commit_message_1_01, true(Message == 'First commit\n')) :-
		fixture_object(Object),
		Object::repository_commit_message(Message).

	test(application_property_1_01, true) :-
		fixture_object(Object),
		Object::property(name(test_application)),
		Object::property(creators(['Tool: Build pipeline', 'Person: Alice Example'])),
		Object::property(repository_commit_author('John Doe')),
		Object::property(repository('https://example.com/example-application.git')).

	test(application_sparse_repository_1_01, false) :-
		sparse_object(Object),
		Object::repository(_).

	test(application_sparse_repository_branch_1_01, false) :-
		sparse_object(Object),
		Object::repository_branch(_).

	test(application_sparse_repository_commit_1_01, false) :-
		sparse_object(Object),
		Object::repository_commit(_).

	test(application_sparse_repository_commit_author_1_01, false) :-
		sparse_object(Object),
		Object::repository_commit_author(_).

	test(application_sparse_repository_commit_message_1_01, false) :-
		sparse_object(Object),
		Object::repository_commit_message(_).

	test(application_sparse_repository_reference_1_01, false) :-
		sparse_object(Object),
		Object::external_reference(repository, _).

	test(application_sparse_property_1_01, false) :-
		sparse_object(Object),
		Object::property(repository(_)).

:- end_object.
