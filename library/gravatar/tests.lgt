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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Unit tests for the "gravatar" library.'
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(gravatar).
	cover(gravatar_api).

	test(gravatar_email_hash_2_01, deterministic(Hash == '84059b07d4be67b806386c0aad8070a23f18836bbaae342275dc0a83414c32ee')) :-
		gravatar::email_hash('MyEmailAddress@example.com ', Hash).

	test(gravatar_email_hash_2_02, error(type_error(atom, 42))) :-
		gravatar::email_hash(42, _Hash).

	test(gravatar_api_profile_path_2_01, deterministic(Path == '/profiles/abc')) :-
		gravatar_api::profile_path(abc, Path).

	test(gravatar_field_3_01, deterministic(Value == 'Ada Lovelace')) :-
		profile(Profile),
		gravatar::display_name(Profile, Value).

	test(gravatar_field_3_02, deterministic(Value == 'https://gravatar.com/ada')) :-
		profile(Profile),
		gravatar::profile_url(Profile, Value).

	test(gravatar_field_3_03, deterministic(Value == [])) :-
		profile(Profile),
		gravatar::verified_accounts(Profile, Value).

	test(gravatar_field_3_04, deterministic(Value == {hidden_contact_info- @false})) :-
		profile(Profile),
		gravatar::section_visibility(Profile, Value).

	test(gravatar_profile_3_01, error(domain_error(option, invalid_option(foo)))) :-
		gravatar::profile('user@example.com', _Profile, [invalid_option(foo)]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(gravatar_profile_3_02, deterministic) :-
			open_test_listener(Port, Listener),
			threaded_once(serve_test_once(Listener), Tag),
			local_base_url(Port, BaseURL),
			gravatar::profile('MyEmailAddress@example.com ', Profile, [base_url(BaseURL), api_key('test-key')]),
			threaded_exit(serve_test_once(Listener), Tag),
			http_socket_process::close_listener(Listener),
			gravatar::hash(Profile, '84059b07d4be67b806386c0aad8070a23f18836bbaae342275dc0a83414c32ee'),
			gravatar::display_name(Profile, 'Ada Lovelace').

		test(gravatar_profile_response_3_01, deterministic) :-
			open_test_listener(Port, Listener),
			threaded_once(serve_test_once(Listener), Tag),
			local_base_url(Port, BaseURL),
			gravatar::profile_response('MyEmailAddress@example.com ', Response, [base_url(BaseURL), api_key('test-key')]),
			threaded_exit(serve_test_once(Listener), Tag),
			http_socket_process::close_listener(Listener),
			status(Response, status(200, 'OK')),
			body(Response, content('application/json', json(Profile))),
			gravatar::company(Profile, 'Analytical Engine').

		open_test_listener(Port, Listener) :-
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []).

		serve_test_once(Listener) :-
			catch(http_socket_process::serve_once(Listener, mock_gravatar_profile_test_handler, _ClientInfo), _, true).

		local_base_url(Port, URL) :-
			atomic_list_concat(['http://127.0.0.1:', Port], URL).

	:- endif.

	profile({
		hash-'abc',
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
	}).

:- end_object.
