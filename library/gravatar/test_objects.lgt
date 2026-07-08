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


:- object(mock_gravatar_profile_test_handler,
	implements(http_handler_protocol),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Mock REST handler used by the gravatar client local tests.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).

	:- protected(profile/2).
	:- mode(profile(+compound, --term), one).

	endpoint(getProfileById, get, '/profiles/{profileIdentifier}', profile, [produces(['application/json'])]).

	profile(Request, ok(Profile)) :-
		::path_parameter(Request, profileIdentifier, Hash),
		::request_header(Request, authorization, 'Bearer test-key'),
		Profile = {
			hash-Hash,
			display_name-'Ada Lovelace',
			profile_url-'https://gravatar.com/ada',
			avatar_url-'https://0.gravatar.com/avatar/test',
			avatar_alt_text-'Ada Lovelace avatar',
			location-'London, UK',
			description-'Mathematician',
			job_title-'Programmer',
			company-'Analytical Engine',
			verified_accounts-[],
			pronunciation-'Ay-duh',
			pronouns-'she/her',
			section_visibility-{hidden_contact_info- @false}
		}.

:- end_object.
