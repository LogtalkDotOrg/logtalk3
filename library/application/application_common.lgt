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


:- category(application_common,
	implements(application_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-03-25,
		comment is 'Application metadata and provenance predicates.'
	]).

	:- uses(os, [
		file_exists/1,
		path_concat/3
	]).

	loader_file(Loader) :-
		self(Self),
		object_property(Self, file(_, Directory)),
		path_concat(Directory, 'loader.lgt', Loader),
		file_exists(Loader).

	external_reference(homepage, URL) :-
		::homepage(URL).
	external_reference(distribution, URL) :-
		::distribution(URL).
	external_reference(package, Identifier) :-
		::package(Identifier).
	external_reference(repository, URL) :-
		::repository(URL).
	external_reference(git_object_identifier, Identifier) :-
		::git_object_identifier(Identifier).
	external_reference(software_heritage_identifier, Identifier) :-
		::software_heritage_identifier(Identifier).

	property(name(Name)) :-
		::name(Name).
	property(version(Version)) :-
		::version(Version).
	property(description(Description)) :-
		::description(Description).
	property(license(License)) :-
		::license(License).
	property(homepage(URL)) :-
		::homepage(URL).
	property(distribution(URL)) :-
		::distribution(URL).
	property(package(Identifier)) :-
		::package(Identifier).
	property(loader_file(File)) :-
		::loader_file(File).
	property(creators(Creators)) :-
		::creators(Creators).
	property(supplier(Supplier)) :-
		::supplier(Supplier).
	property(originator(Originator)) :-
		::originator(Originator).
	property(built_date(Date)) :-
		::built_date(Date).
	property(release_date(Date)) :-
		::release_date(Date).
	property(valid_until_date(Date)) :-
		::valid_until_date(Date).
	property(external_reference(Type, URL)) :-
		::external_reference(Type, URL).
	property(repository(URL)) :-
		::repository(URL).
	property(repository_branch(Branch)) :-
		::repository_branch(Branch).
	property(repository_commit(Hash)) :-
		::repository_commit(Hash).
	property(repository_commit_abbreviated(Hash)) :-
		::repository_commit_abbreviated(Hash).
	property(repository_commit_date(Date)) :-
		::repository_commit_date(Date).
	property(repository_commit_author(Author)) :-
		::repository_commit_author(Author).
	property(repository_commit_message(Message)) :-
		::repository_commit_message(Message).
	property(git_object_identifier(Identifier)) :-
		::git_object_identifier(Identifier).
	property(software_heritage_identifier(Identifier)) :-
		::software_heritage_identifier(Identifier).

:- end_category.
