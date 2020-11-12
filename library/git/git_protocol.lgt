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


:- protocol(git_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-11-12,
		comment is 'Predicates for accessing a git project current branch and latest commit data.'
	]).

	:- public(branch/2).
	:- mode(branch(+atom, ?atom), zero_or_one).
	:- info(branch/2, [
		comment is 'Returns the name of the current git branch.',
		argnames is ['Directory', 'Branch']
	]).

	:- public(commit_author/2).
	:- mode(commit_author(+atom, -atom), zero_or_one).
	:- info(commit_author/2, [
		comment is 'Returns the latest commit author.',
		argnames is ['Directory', 'Author']
	]).

	:- public(commit_date/2).
	:- mode(commit_date(+atom, -atom), zero_or_one).
	:- info(commit_date/2, [
		comment is 'Returns the latest commit date (strict ISO 8601 format).',
		argnames is ['Directory', 'Date']
	]).

	:- public(commit_hash/2).
	:- mode(commit_hash(+atom, -atom), zero_or_one).
	:- info(commit_hash/2, [
		comment is 'Returns the latest commit hash.',
		argnames is ['Directory', 'Hash']
	]).

	:- public(commit_hash_abbreviated/2).
	:- mode(commit_hash_abbreviated(+atom, -atom), zero_or_one).
	:- info(commit_hash_abbreviated/2, [
		comment is 'Returns the latest commit abbreviated hash.',
		argnames is ['Directory', 'Hash']
	]).

	:- public(commit_message/2).
	:- mode(commit_message(+atom, -atom), zero_or_one).
	:- info(commit_message/2, [
		comment is 'Returns the latest commit message.',
		argnames is ['Directory', 'Message']
	]).

	:- public(commit_log/3).
	:- mode(commit_log(+atom, +atom, -atom), zero_or_one).
	:- info(commit_log/3, [
		comment is 'Returns the git latest commit log output for the given format (see e.g. https://git-scm.com/docs/pretty-formats).',
		argnames is ['Directory', 'Format', 'Output']
	]).

:- end_protocol.
