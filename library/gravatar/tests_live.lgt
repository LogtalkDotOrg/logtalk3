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


:- object(tests_live,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Optional live tests for the "gravatar" library using the public Gravatar API.'
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	:- uses(os, [
		environment_variable/2
	]).

	cover(gravatar).

	condition :-
		environment_variable('LOGTALK_GRAVATAR_API_KEY', APIKey),
		APIKey \== ''.

	test(gravatar_profile_3_live_01, deterministic) :-
		gravatar::profile('pmoura@logtalk.org', Profile, []),
		gravatar::hash(Profile, 'aeb49dc80c446b00b145e7172b7a534d20963a08316cd8bcb65cad24ca2ca48f'),
		gravatar::display_name(Profile, DisplayName),
		gravatar::profile_url(Profile, ProfileURL),
		gravatar::avatar_url(Profile, AvatarURL),
		atom(DisplayName),
		atom(ProfileURL),
		atom(AvatarURL).

	test(gravatar_profile_response_3_live_01, deterministic) :-
		gravatar::profile_response('pmoura@logtalk.org', Response, []),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json(Profile))),
		gravatar::hash(Profile, 'aeb49dc80c446b00b145e7172b7a534d20963a08316cd8bcb65cad24ca2ca48f').

:- end_object.

