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


:- object(gravatar_api,
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'REST metadata and path construction helpers for the Gravatar public API.'
	]).

	:- public(profile_path/2).
	:- mode(profile_path(+atom, -atom), one).
	:- info(profile_path/2, [
		comment is 'Builds the profile endpoint path for a SHA-256 email hash or profile URL slug.',
		argnames is ['ProfileIdentifier', 'Path']
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'REST endpoint descriptor for the Gravatar profile lookup operation.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(profile/2).
	:- mode(profile(+compound, --term), one).
	:- info(profile/2, [
		comment is 'Placeholder action for the remote Gravatar profile endpoint descriptor.',
		argnames is ['Request', 'Result']
	]).

	profile_path(ProfileIdentifier, Path) :-
		atom_concat('/profiles/', ProfileIdentifier, Path).

	endpoint(
		getProfileById,
		get,
		'/profiles/{profileIdentifier}',
		profile,
		[
			summary('Get profile by identifier'),
			description('Returns a Gravatar profile by SHA-256 email hash or profile URL slug.'),
			tags([profiles]),
			produces(['application/json'])
		]
	).

	profile(_Request, problem(501, 'urn:logtalk:remote-rest-endpoint', 'Not Implemented', 'This object describes a remote Gravatar REST endpoint. Use gravatar::profile/2-3 to call it.')).

:- end_object.

