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


:- protocol(application_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-24,
		comment is 'Application metadata predicates, including optional source provenance facts.',
		remarks is [
			'Explicit metadata' - 'All predicates describe metadata declared by the application. The protocol does not require or imply reflection over the current and transient state of some application repository.',
			'Release metadata' - 'Predicates such as ``name/1``, ``version/1``, ``description/1``, ``license/1``, ``distribution/1``, ``release_date/1``, and ``valid_until_date/1`` are intended for release-oriented metadata.',
			'Source provenance metadata' - 'Predicates such as ``repository/1``, ``repository_branch/1``, ``repository_commit/1``, ``repository_commit_date/1``, ``repository_commit_author/1``, and ``repository_commit_message/1`` are optional explicit provenance facts about the source used for the application and may or may not correspond to the release artifact identity.'
		]
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_one).
	:- info(name/1, [
		comment is 'Application name.',
		argnames is ['Name']
	]).

	:- public(version/1).
	:- mode(version(?atom), zero_or_one).
	:- info(version/1, [
		comment is 'Application version.',
		argnames is ['Version']
	]).

	:- public(description/1).
	:- mode(description(?atom), zero_or_one).
	:- info(description/1, [
		comment is 'Application short description.',
		argnames is ['Description']
	]).

	:- public(license/1).
	:- mode(license(?atom), zero_or_one).
	:- info(license/1, [
		comment is 'Application license.',
		argnames is ['License']
	]).

	:- public(homepage/1).
	:- mode(homepage(?atom), zero_or_one).
	:- info(homepage/1, [
		comment is 'Application homepage URL.',
		argnames is ['URL']
	]).

	:- public(distribution/1).
	:- mode(distribution(?atom), zero_or_one).
	:- info(distribution/1, [
		comment is 'Application distribution or download location.',
		argnames is ['URL']
	]).

	:- public(loader_file/1).
	:- mode(loader_file(?atom), zero_or_one).
	:- info(loader_file/1, [
		comment is 'Application main loader file absolute path.',
		argnames is ['File']
	]).

	:- public(creators/1).
	:- mode(creators(?list(atom)), zero_or_one).
	:- info(creators/1, [
		comment is 'Application creators, authors, or other credited producers of the application or its release metadata.',
		argnames is ['Creators']
	]).

	:- public(supplier/1).
	:- mode(supplier(?atom), zero_or_one).
	:- info(supplier/1, [
		comment is 'Application supplier.',
		argnames is ['Supplier']
	]).

	:- public(originator/1).
	:- mode(originator(?atom), zero_or_one).
	:- info(originator/1, [
		comment is 'Original source of the application software when distinct from its creators.',
		argnames is ['Originator']
	]).

	:- public(built_date/1).
	:- mode(built_date(?atom), zero_or_one).
	:- info(built_date/1, [
		comment is 'Application build date.',
		argnames is ['Date']
	]).

	:- public(release_date/1).
	:- mode(release_date(?atom), zero_or_one).
	:- info(release_date/1, [
		comment is 'Application release date.',
		argnames is ['Date']
	]).

	:- public(valid_until_date/1).
	:- mode(valid_until_date(?atom), zero_or_one).
	:- info(valid_until_date/1, [
		comment is 'Application validity limit date.',
		argnames is ['Date']
	]).

	:- public(external_reference/2).
	:- mode(external_reference(?atom, ?atom), zero_or_more).
	:- info(external_reference/2, [
		comment is 'Application explicit external references using the same vocabulary as the corresponding first-class metadata predicates.',
		argnames is ['Type', 'URL']
	]).

	:- public(repository/1).
	:- mode(repository(?atom), zero_or_one).
	:- info(repository/1, [
		comment is 'Application source provenance repository metadata.',
		argnames is ['URL']
	]).

	:- public(repository_branch/1).
	:- mode(repository_branch(?atom), zero_or_one).
	:- info(repository_branch/1, [
		comment is 'Application source provenance git branch metadata.',
		argnames is ['Branch']
	]).

	:- public(repository_commit/1).
	:- mode(repository_commit(?atom), zero_or_one).
	:- info(repository_commit/1, [
		comment is 'Application source provenance git commit metadata.',
		argnames is ['Hash']
	]).

	:- public(repository_commit_abbreviated/1).
	:- mode(repository_commit_abbreviated(?atom), zero_or_one).
	:- info(repository_commit_abbreviated/1, [
		comment is 'Application abbreviated source provenance git commit metadata.',
		argnames is ['Hash']
	]).

	:- public(repository_commit_date/1).
	:- mode(repository_commit_date(?atom), zero_or_one).
	:- info(repository_commit_date/1, [
		comment is 'Application source provenance git commit date metadata.',
		argnames is ['Date']
	]).

	:- public(repository_commit_author/1).
	:- mode(repository_commit_author(?atom), zero_or_one).
	:- info(repository_commit_author/1, [
		comment is 'Application source provenance git commit author metadata.',
		argnames is ['Author']
	]).

	:- public(repository_commit_message/1).
	:- mode(repository_commit_message(?atom), zero_or_one).
	:- info(repository_commit_message/1, [
		comment is 'Application source provenance git commit message metadata.',
		argnames is ['Message']
	]).

	:- public(property/1).
	:- mode(property(?compound), zero_or_more).
	:- info(property/1, [
		comment is 'Enumerates declared application metadata and optional source provenance as individual property terms.',
		argnames is ['Property']
	]).

:- end_protocol.
