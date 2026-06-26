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


:- object(gravatar,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Portable Gravatar profile client using the Gravatar REST API.'
	]).

	:- public(profile/2).
	:- mode(profile(+atom, -term), one_or_error).
	:- info(profile/2, [
		comment is 'Retrieves a Gravatar profile JSON object for an email address using default options.',
		argnames is ['Email', 'Profile'],
		exceptions is [
			'``Email`` is a variable' - instantiation_error,
			'``Email`` is neither a variable nor an atom' - type_error(atom, 'Email'),
			'The Gravatar API returns a non-success response' - gravatar_api_error('Status', 'Body')
		]
	]).

	:- public(profile/3).
	:- mode(profile(+atom, -term, +list(compound)), one_or_error).
	:- info(profile/3, [
		comment is 'Retrieves a Gravatar profile JSON object for an email address using the given options.',
		argnames is ['Email', 'Profile', 'Options'],
		remarks is [
			'Option ``api_key(APIKey)``' - 'Uses the given Gravatar API key as a Bearer token. When absent, the ``LOGTALK_GRAVATAR_API_KEY`` environment variable is used when defined.',
			'Option ``base_url(URL)``' - 'Overrides the Gravatar API base URL. The default is ``https://api.gravatar.com/v3``.',
			'HTTP options' - 'Options ``headers/1``, ``version/1``, ``properties/1``, and ``connection_options/1`` are forwarded to the HTTP client.'
		],
		exceptions is [
			'``Email`` is a variable' - instantiation_error,
			'``Email`` is neither a variable nor an atom' - type_error(atom, 'Email'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The Gravatar API returns a non-success response' - gravatar_api_error('Status', 'Body')
		]
	]).

	:- public(profile_response/3).
	:- mode(profile_response(+atom, -compound, +list(compound)), one_or_error).
	:- info(profile_response/3, [
		comment is 'Retrieves the raw normalized HTTP response for a Gravatar profile request.',
		argnames is ['Email', 'Response', 'Options']
	]).

	:- public(email_hash/2).
	:- mode(email_hash(+atom, -atom), one_or_error).
	:- info(email_hash/2, [
		comment is 'Computes the Gravatar SHA-256 email hash after trimming leading and trailing whitespace and lowercasing ASCII letters.',
		argnames is ['Email', 'Hash'],
		exceptions is [
			'``Email`` is a variable' - instantiation_error,
			'``Email`` is neither a variable nor an atom' - type_error(atom, 'Email')
		]
	]).

	:- public(field/3).
	:- mode(field(+term, +atom, ?term), zero_or_one).
	:- info(field/3, [
		comment is 'Returns a top-level field from a decoded Gravatar profile JSON object.',
		argnames is ['Profile', 'Field', 'Value']
	]).

	:- public(hash/2).
	:- mode(hash(+term, ?term), zero_or_one).
	:- info(hash/2, [
		comment is 'Returns the Gravatar profile hash field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(display_name/2).
	:- mode(display_name(+term, ?term), zero_or_one).
	:- info(display_name/2, [
		comment is 'Returns the Gravatar profile display name field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(profile_url/2).
	:- mode(profile_url(+term, ?term), zero_or_one).
	:- info(profile_url/2, [
		comment is 'Returns the Gravatar profile URL field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(avatar_url/2).
	:- mode(avatar_url(+term, ?term), zero_or_one).
	:- info(avatar_url/2, [
		comment is 'Returns the Gravatar avatar URL field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(avatar_alt_text/2).
	:- mode(avatar_alt_text(+term, ?term), zero_or_one).
	:- info(avatar_alt_text/2, [
		comment is 'Returns the Gravatar avatar alternative text field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(location/2).
	:- mode(location(+term, ?term), zero_or_one).
	:- info(location/2, [
		comment is 'Returns the Gravatar profile location field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(job_title/2).
	:- mode(job_title(+term, ?term), zero_or_one).
	:- info(job_title/2, [
		comment is 'Returns the Gravatar profile job title field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(company/2).
	:- mode(company(+term, ?term), zero_or_one).
	:- info(company/2, [
		comment is 'Returns the Gravatar profile company field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(description/2).
	:- mode(description(+term, ?term), zero_or_one).
	:- info(description/2, [
		comment is 'Returns the Gravatar profile description field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(pronouns/2).
	:- mode(pronouns(+term, ?term), zero_or_one).
	:- info(pronouns/2, [
		comment is 'Returns the Gravatar profile pronouns field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(verified_accounts/2).
	:- mode(verified_accounts(+term, ?term), zero_or_one).
	:- info(verified_accounts/2, [
		comment is 'Returns the Gravatar profile verified accounts field.',
		argnames is ['Profile', 'Value']
	]).

	:- public(section_visibility/2).
	:- mode(section_visibility(+term, ?term), zero_or_one).
	:- info(section_visibility/2, [
		comment is 'Returns the Gravatar profile section visibility field.',
		argnames is ['Profile', 'Value']
	]).

	:- uses(list, [
		member/2, memberchk/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(os, [
		environment_variable/2
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	profile(Email, Profile) :-
		profile(Email, Profile, []).

	profile(Email, Profile, Options) :-
		profile_response(Email, Response, Options),
		(	status(Response, status(200, _ReasonPhrase)) ->
			profile_json(Response, Profile)
		;	api_error(Response)
		).

	profile_response(Email, Response, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		email_hash(Email, Hash),
		operation_url(Hash, URL, MergedOptions),
		http_options(Options, MergedOptions, HTTPOptions),
		http_client(http_socket_process)::get(URL, Response, HTTPOptions).

	email_hash(Email, Hash) :-
		normalized_email(Email, NormalizedEmail),
		atom_codes(NormalizedEmail, Bytes),
		sha256::hash(Bytes, Hash).

	field({Pairs}, Field, Value) :-
		!,
		object_field(Pairs, Field, Value).
	field(json(Pairs), Field, Value) :-
		!,
		memberchk(Field-Value, Pairs).
	field(Pairs, Field, Value) :-
		proper_list(Pairs),
		memberchk(Field-Value, Pairs).

	hash(Profile, Value) :-
		field(Profile, hash, Value).

	display_name(Profile, Value) :-
		field(Profile, display_name, Value).

	profile_url(Profile, Value) :-
		field(Profile, profile_url, Value).

	avatar_url(Profile, Value) :-
		field(Profile, avatar_url, Value).

	avatar_alt_text(Profile, Value) :-
		field(Profile, avatar_alt_text, Value).

	location(Profile, Value) :-
		field(Profile, location, Value).

	job_title(Profile, Value) :-
		field(Profile, job_title, Value).

	company(Profile, Value) :-
		field(Profile, company, Value).

	description(Profile, Value) :-
		field(Profile, description, Value).

	pronouns(Profile, Value) :-
		field(Profile, pronouns, Value).

	verified_accounts(Profile, Value) :-
		field(Profile, verified_accounts, Value).

	section_visibility(Profile, Value) :-
		field(Profile, section_visibility, Value).

	operation_url(ProfileIdentifier, URL, Options) :-
		^^option(base_url(BaseURL0), Options, base_url('https://api.gravatar.com/v3')),
		base_url(BaseURL0, BaseURL),
		gravatar_api::profile_path(ProfileIdentifier, Path),
		atom_concat(BaseURL, Path, URL),
		(	url(atom)::valid(URL) ->
			true
		;	domain_error(gravatar_url, URL)
		).

	http_options(UserOptions, Options, HTTPOptions) :-
		^^option(headers(Headers0), Options, headers([])),
		request_headers(UserOptions, Headers0, Headers),
		http_options_(Options, HTTPOptions0),
		HTTPOptions = [headers(Headers)| HTTPOptions0].

	http_options_([], []).
	http_options_([Option| Options], HTTPOptions) :-
		(	http_option(Option) ->
			HTTPOptions = [Option| HTTPOptionsTail],
			http_options_(Options, HTTPOptionsTail)
		;	gravatar_option(Option) ->
			http_options_(Options, HTTPOptions)
		;	domain_error(gravatar_option, Option)
		).

	http_option(version(_)).
	http_option(properties(_)).
	http_option(connection_options(_)).

	gravatar_option(base_url(_)).
	gravatar_option(api_key(_)).
	gravatar_option(headers(_)).

	request_headers(Options, Headers0, Headers) :-
		default_header(accept-'application/json', Headers0, Headers1),
		api_key_header(Options, Headers1, Headers).

	api_key_header(Options, Headers0, Headers) :-
		(	api_key(Options, APIKey) ->
			atom_concat('Bearer ', APIKey, Authorization),
			default_header(authorization-Authorization, Headers0, Headers)
		;	Headers = Headers0
		).

	api_key(Options, APIKey) :-
		member(api_key(APIKey), Options),
		!.
	api_key(_Options, APIKey) :-
		environment_variable('LOGTALK_GRAVATAR_API_KEY', APIKey),
		APIKey \== ''.

	default_header(Name-_, Headers, Headers) :-
		member(Name-_, Headers),
		!.
	default_header(Header, Headers, [Header| Headers]).

	profile_json(Response, Profile) :-
		body(Response, Body),
		(	Body = content(_MediaType, json(Profile)) ->
			true
		;	domain_error(gravatar_profile_response_body, Body)
		).

	api_error(Response) :-
		status(Response, Status),
		body(Response, Body),
		context(Context),
		throw(error(gravatar_api_error(Status, Body), Context)).

	base_url(BaseURL0, BaseURL) :-
		(	sub_atom(BaseURL0, _, 1, 0, '/') ->
			sub_atom(BaseURL0, 0, _, 1, BaseURL1)
		;	BaseURL1 = BaseURL0
		),
		BaseURL = BaseURL1.

	normalized_email(Email, NormalizedEmail) :-
		(	var(Email) ->
			instantiation_error
		;	\+ atom(Email) ->
			type_error(atom, Email)
		;	atom_codes(Email, Codes0),
			trim_ascii_whitespace(Codes0, Codes1),
			lowercase_ascii_codes(Codes1, Codes),
			atom_codes(NormalizedEmail, Codes)
		).

	trim_ascii_whitespace(Codes0, Codes) :-
		trim_ascii_whitespace_left(Codes0, Codes1),
		reverse(Codes1, ReversedCodes1),
		trim_ascii_whitespace_left(ReversedCodes1, ReversedCodes),
		reverse(ReversedCodes, Codes).

	trim_ascii_whitespace_left([Code| Codes0], Codes) :-
		ascii_whitespace(Code),
		!,
		trim_ascii_whitespace_left(Codes0, Codes).
	trim_ascii_whitespace_left(Codes, Codes).

	lowercase_ascii_codes([], []).
	lowercase_ascii_codes([Code0| Codes0], [Code| Codes]) :-
		lowercase_ascii_code(Code0, Code),
		lowercase_ascii_codes(Codes0, Codes).

	lowercase_ascii_code(Code0, Code) :-
		Code0 >= 0'A,
		Code0 =< 0'Z,
		!,
		Code is Code0 + 32.
	lowercase_ascii_code(Code, Code).

	ascii_whitespace(32).
	ascii_whitespace(0'\t).
	ascii_whitespace(0'\n).
	ascii_whitespace(0'\r).
	ascii_whitespace(0'\v).
	ascii_whitespace(0'\f).

	object_field((Field-Value, _), Field, Value) :-
		!.
	object_field((_, Pairs), Field, Value) :-
		!,
		object_field(Pairs, Field, Value).
	object_field(Field-Value, Field, Value).

	valid_option(api_key(APIKey)) :-
		atom(APIKey),
		APIKey \== ''.
	valid_option(base_url(URL)) :-
		atom(URL),
		url(atom)::valid(URL).
	valid_option(headers(Headers)) :-
		proper_list(Headers).
	valid_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0.
	valid_option(properties(Properties)) :-
		proper_list(Properties).
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions).

	default_option(base_url('https://api.gravatar.com/v3')).
	default_option(headers([])).
	default_option(version(http(1, 1))).
	default_option(properties([])).
	default_option(connection_options([])).

:- end_object.
