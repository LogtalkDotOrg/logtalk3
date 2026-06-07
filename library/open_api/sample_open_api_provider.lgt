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


:- object(sample_open_api_provider,
	implements(open_api_provider_protocol),
	imports(application_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Sample OpenAPI provider object used for documentation examples and unit tests.'
	]).

	api_info(info('Users API', '1.0.0', 'User management HTTP API', [])).

	servers([
		server('https://api.example.com/v1', 'Production server'),
		server('https://staging-api.example.com/v1', 'Staging server')
	]).

	security([[user_oauth-[read_users]]]).

	operations([
		operation(
			create_user,
			post,
			'/users',
			'Create a user',
			[],
			request_body(
				'User to create',
				true,
				[media('application/json', {
					type-object,
					properties-{
						name-{type-string, minLength-1, maxLength-100},
						email-{type-string, format-email},
						active-{type-boolean}
					},
					required-[name, email],
					additionalProperties- @false
				})]
			),
			[
				response(201, 'Created user', [media('application/json', schema_ref(user))]),
				response(default, 'Error response', [media('application/json', schema_ref(api_error))])
			],
			[
				description('Creates a new user and returns the created representation.'),
				tags([users])
			]
		),
		operation(
			get_user,
			get,
			'/users/{id}',
			'Get a user',
			[
				parameter(id, path, 'User identifier', true, schema_ref(user_id))
			],
			none,
			[
				response(200, 'Requested user', [media('application/json', schema_ref(user))]),
				response(default, 'Error response', [media('application/json', schema_ref(api_error))])
			],
			[
				description('Returns the user identified by the given id.'),
				tags([users])
			]
		),
		operation(
			update_user,
			put,
			'/users/{id}',
			'Update a user',
			[
				parameter(id, path, 'User identifier', true, schema_ref(user_id)),
				parameter(verbose, query, 'Include expanded details in the response', false, {type-boolean})
			],
			request_body('User fields to update', true, [media('application/json', schema_ref(user_update_request))]),
			[
				response(200, 'Updated user', [media('application/json', schema_ref(user))]),
				response(default, 'Error response', [media('application/json', schema_ref(api_error))])
			],
			[
				description('Updates an existing user and optionally returns expanded details.'),
				tags([users]),
				deprecated(false),
				security([[], [user_bearer-[]]])
			]
		)
	]).

	description('Example OpenAPI provider object for request, response, and document validation.').

	license('Apache-2.0').

	homepage('https://example.com/apis/users').

	security_scheme(user_bearer, http(bearer, [bearer_format('JWT')])).

	security_scheme(user_oauth, oauth2([
		client_credentials(
			'https://auth.example.com/oauth/token',
			[
				scope(read_users, 'Read user data'),
				scope(write_users, 'Write user data')
			]
		)
	])).

	schema(user_id, {
		type-string,
		format-uuid
	}).

	schema(user_update_request, {
		type-object,
		properties-{
			name-{type-string, minLength-1, maxLength-100},
			email-{type-string, format-email},
			active-{type-boolean}
		},
		minProperties-1,
		additionalProperties- @false
	}).

	schema(user, {
		type-object,
		properties-{
			id-{type-string, format-uuid},
			name-{type-string, minLength-1, maxLength-100},
			email-{type-string, format-email},
			active-{type-boolean},
			updated_at-{type-string, format-'date-time'}
		},
		required-[id, name, email, active, updated_at],
		additionalProperties- @false
	}).

	schema(api_error, {
		type-object,
		properties-{
			code-{type-string, enum-[invalid_request, validation_error, not_found, conflict, internal_error]},
			message-{type-string, minLength-1},
			details-{type-array, items-{type-string}}
		},
		required-[code, message],
		additionalProperties- @false
	}).

:- end_object.
