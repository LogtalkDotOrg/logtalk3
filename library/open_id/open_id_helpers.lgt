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


:- category(open_id_helpers,
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-06,
		comment is 'Internal shared helpers for the open_id library.'
	]).

	:- protected(json_member/3).
	:- mode(json_member(+atom, +term, -term), zero_or_more).
	:- info(json_member/3, [
		comment is 'Enumerates JSON object member values for a given key.',
		argnames is ['Key', 'Object', 'Value']
	]).

	:- protected(json_member_default/4).
	:- mode(json_member_default(+atom, +term, +term, -term), one).
	:- info(json_member_default/4, [
		comment is 'Returns a JSON object member value or a default when the key is absent.',
		argnames is ['Key', 'Object', 'Default', 'Value']
	]).

	:- protected(provider_property/3).
	:- mode(provider_property(+compound, +atom, -term), zero_or_one).
	:- info(provider_property/3, [
		comment is 'Looks up a property in a provider metadata compound term.',
		argnames is ['Provider', 'Name', 'Value']
	]).

	:- protected(provider_property_default/4).
	:- mode(provider_property_default(+compound, +atom, +term, -term), one).
	:- info(provider_property_default/4, [
		comment is 'Returns a provider metadata property or a default when the property is absent.',
		argnames is ['Provider', 'Name', 'Default', 'Value']
	]).

	:- protected(normalize_options_term/2).
	:- mode(normalize_options_term(+term, -list(compound)), one).
	:- info(normalize_options_term/2, [
		comment is 'Normalizes either a direct options list or a named options wrapper term.',
		argnames is ['NameOptions', 'Options']
	]).

	:- protected(query_atom/2).
	:- mode(query_atom(+list(pair), -atom), one_or_error).
	:- info(query_atom/2, [
		comment is 'Encodes query parameters as a URL query atom.',
		argnames is ['Pairs', 'Query'],
		exceptions is [
			'The HTTP body sink is a variable' - instantiation_error,
			'The HTTP body sink is not a valid sink term' - domain_error(http_sink, 'Sink'),
			'``Pairs`` cannot be represented as an HTTP form body' - domain_error(http_body, 'Body'),
			'The HTTP body options are invalid' - domain_error(http_body_options, 'Options')
		]
	]).

	:- protected(space_atom/2).
	:- mode(space_atom(+term, -atom), one_or_error).
	:- info(space_atom/2, [
		comment is 'Converts an atom or list of atoms into a space-separated atom.',
		argnames is ['Value', 'Atom'],
		exceptions is [
			'``Value`` is neither an atom nor a list of atoms' - domain_error(open_id_space_separated_atom, 'Value')
		]
	]).

	:- protected(url_scheme/2).
	:- mode(url_scheme(+atom, -atom), zero_or_one).
	:- info(url_scheme/2, [
		comment is 'Extracts the scheme from an absolute URL atom.',
		argnames is ['URL', 'Scheme']
	]).

	:- protected(https_url/1).
	:- mode(https_url(+atom), zero_or_one).
	:- info(https_url/1, [
		comment is 'Succeeds when the URL uses the https scheme.',
		argnames is ['URL']
	]).

	:- protected(localhost_http_url/1).
	:- mode(localhost_http_url(+atom), zero_or_one).
	:- info(localhost_http_url/1, [
		comment is 'Succeeds when the URL uses http for a localhost or loopback host.',
		argnames is ['URL']
	]).

	:- protected(ensure_absolute_url/2).
	:- mode(ensure_absolute_url(+atom, +atom), one_or_error).
	:- info(ensure_absolute_url/2, [
		comment is 'Validates that a value is an absolute URL for the given domain label.',
		argnames is ['Domain', 'URL'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not an absolute URL atom' - domain_error('Domain', 'URL')
		]
	]).

	:- protected(ensure_secure_url/3).
	:- mode(ensure_secure_url(+atom, +atom, +list(compound)), one_or_error).
	:- info(ensure_secure_url/3, [
		comment is 'Validates that a URL uses https or an allowed localhost exception.',
		argnames is ['Domain', 'URL', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not an absolute URL atom' - domain_error('Domain', 'URL'),
			'``URL`` is not secure and is not an allowed localhost HTTP URL' - domain_error('Domain', 'URL')
		]
	]).

	:- protected(atom_bytes/2).
	:- mode(atom_bytes(+atom, -list(byte)), one).
	:- mode(atom_bytes(-atom, +list(byte)), one).
	:- info(atom_bytes/2, [
		comment is 'Converts between an atom and its byte list representation.',
		argnames is ['Atom', 'Bytes']
	]).

	:- protected(bytes_atom/2).
	:- mode(bytes_atom(+list(byte), -atom), one).
	:- info(bytes_atom/2, [
		comment is 'Converts a byte list into an atom.',
		argnames is ['Bytes', 'Atom']
	]).

	:- protected(base64url_atom_bytes/2).
	:- mode(base64url_atom_bytes(+atom, -list(byte)), one_or_error).
	:- mode(base64url_atom_bytes(-atom, +list(byte)), one_or_error).
	:- info(base64url_atom_bytes/2, [
		comment is 'Converts between unpadded Base64URL atoms and byte lists.',
		argnames is ['Atom', 'Bytes'],
		exceptions is [
			'``Atom`` is a variable or the sink is a variable' - instantiation_error,
			'``Atom`` is neither a variable nor a valid Base64URL source term' - domain_error(base64url_source, codes('Codes')),
			'``Bytes`` cannot be encoded in the requested Base64URL sink representation' - domain_error(base64url_sink, codes('Codes')),
			'``Atom`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- protected(json_object_pairs/2).
	:- mode(json_object_pairs(+term, -list(compound)), zero_or_one).
	:- info(json_object_pairs/2, [
		comment is 'Extracts the key-value pairs from a JSON object term.',
		argnames is ['Object', 'Pairs']
	]).

	:- protected(pair_key_value/3).
	:- mode(pair_key_value(+compound, -term, -term), one).
	:- info(pair_key_value/3, [
		comment is 'Extracts the key and value from a JSON pair compound term.',
		argnames is ['Pair', 'Key', 'Value']
	]).

	:- protected(append_query/3).
	:- mode(append_query(+atom, +atom, -atom), one).
	:- info(append_query/3, [
		comment is 'Appends a query atom to an endpoint URL, preserving any existing query.',
		argnames is ['Endpoint', 'Query', 'URL']
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	valid_option(allow_insecure_http(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(client_authentication(Authentication)) :-
		ground(Authentication),
		valid_client_authentication(Authentication).
	valid_option(clock_skew(ClockSkew)) :-
		number(ClockSkew),
		ClockSkew >= 0.
	valid_option(now(Now)) :-
		number(Now).
	valid_option(expected_audience(Audience)) :-
		atom(Audience).
	valid_option(expected_nonce(Nonce)) :-
		atom(Nonce).
	valid_option(client_id(ClientId)) :-
		atom(ClientId).
	valid_option(required_claims(RequiredClaims)) :-
		atom_list(RequiredClaims).
	valid_option(allow_algorithms(Algorithms)) :-
		atom_list(Algorithms).
	valid_option(code_verifier(Verifier)) :-
		atom(Verifier).
	valid_option(jwks_cache_ttl(TTL)) :-
		number(TTL),
		TTL >= 0.
	valid_option(openssl_executable(Executable)) :-
		atom(Executable).
	valid_option(openssl_arguments(Arguments)) :-
		list::valid(Arguments).
	valid_option(refresh_on_unknown_kid(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(server_name(ServerName)) :-
		atom(ServerName).
	valid_option(connection_options(ConnectionOptions)) :-
		list::valid(ConnectionOptions).
	valid_option(headers(Headers)) :-
		list::valid(Headers).
	valid_option(query(Query)) :-
		list::valid(Query).
	valid_option(scope(Scope)) :-
		(	atom(Scope) ->
			true
		;	atom_list(Scope)
		).
	valid_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0.
	valid_option(properties(Properties)) :-
		list::valid(Properties).
	valid_option(state(State)) :-
		atom(State).
	valid_option(nonce(Nonce)) :-
		atom(Nonce).
	valid_option(provider(Properties)) :-
		list::valid(Properties).
	valid_option(session(Properties)) :-
		list::valid(Properties).
	valid_option(allow_fragment_response(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	default_option(allow_insecure_http(false)).
	default_option(client_authentication(none)).
	default_option(clock_skew(60)).
	default_option(jwks_cache_ttl(3600)).
	default_option(refresh_on_unknown_kid(true)).
	default_option(required_claims([])).
	default_option(allow_algorithms(['RS256', 'ES256'])).

	json_member(Key, Object, Value) :-
		json_object_pairs(Object, Pairs),
		json_pair_member(Pairs, Key, Value).

	json_member_default(Key, Object, Default, Value) :-
		(	json_member(Key, Object, Value0) ->
			Value = Value0
		;	Value = Default
		).

	json_pair_member([Pair| _], Key, Value) :-
		pair_key_value(Pair, Key, Value),
		!.
	json_pair_member([_| Pairs], Key, Value) :-
		json_pair_member(Pairs, Key, Value).

	provider_property(provider(Properties), Name, Value) :-
		Term =.. [Name, Value],
		^^option(Term, Properties).

	provider_property_default(Provider, Name, Default, Value) :-
		(	provider_property(Provider, Name, Value0) ->
			Value = Value0
		;	Value = Default
		).

	normalize_options_term(NameOptions, Options) :-
		nonvar(NameOptions),
		NameOptions =.. [_Name, Options],
		!,
		(	list::valid(Options) ->
			true
		;	domain_error(open_id_options, Options)
		).
	normalize_options_term(Options, Options) :-
		list::valid(Options),
		!.
	normalize_options_term(Options, _) :-
		(	var(Options) ->
			instantiation_error
		;	domain_error(open_id_options, Options)
		).

	query_atom(Pairs, Query) :-
		http_core::encode_body('application/x-www-form-urlencoded', Pairs, [], Body),
		http_core::generate_body(atom(Query), Body, []).

	space_atom(Atom, Atom) :-
		atom(Atom),
		!.
	space_atom(List, Atom) :-
		list::valid(List),
		!,
		atomic_list_concat(List, ' ', Atom).
	space_atom(Term, _) :-
		domain_error(open_id_space_separated_atom, Term).

	url_scheme(URL, Scheme) :-
		url(atom)::parse(URL, Components),
		memberchk(scheme(Scheme), Components).

	https_url(URL) :-
		url_scheme(URL, https).

	localhost_http_url(URL) :-
		url(atom)::parse(URL, Components),
		member(scheme(http), Components),
		member(authority(Authority), Components),
		(	Authority == 'localhost'
		;	sub_atom(Authority, 0, _, _, 'localhost:')
		;	Authority == '127.0.0.1'
		;	sub_atom(Authority, 0, _, _, '127.0.0.1:')
		;	Authority == '[::1]'
		;	sub_atom(Authority, 0, _, _, '[::1]:')
		).

	ensure_absolute_url(Domain, URL) :-
		(	var(URL) ->
			instantiation_error
		;	atom(URL),
			url_scheme(URL, _) ->
			true
		;	domain_error(Domain, URL)
		).

	ensure_secure_url(Domain, URL, Options) :-
		ensure_absolute_url(Domain, URL),
		(	https_url(URL) ->
			true
		;	^^option(allow_insecure_http(true), Options),
			localhost_http_url(URL) ->
			true
		;	domain_error(Domain, URL)
		).

	atom_bytes(Atom, Bytes) :-
		atom_codes(Atom, Bytes).

	bytes_atom(Bytes, Atom) :-
		atom_codes(Atom, Bytes).

	base64url_atom_bytes(Atom, Bytes) :-
		atom(Atom),
		!,
		atom_codes(Atom, Codes),
		base64url_no_padding::parse(codes(Codes), Bytes).
	base64url_atom_bytes(Atom, Bytes) :-
		base64url_no_padding::generate(codes(Codes), Bytes),
		atom_codes(Atom, Codes).

	json_object_pairs({}, []) :-
		!.
	json_object_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_pairs(json(Pairs), Pairs) :-
		!.

	curly_pairs_to_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		curly_pairs_to_list(Rest, Pairs).
	curly_pairs_to_list(Pair, [Pair]).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

	append_query(Endpoint, Query, URL) :-
		(	sub_atom(Endpoint, _, _, _, '?') ->
			atom_concat(Endpoint, '&', Prefix)
		;	atom_concat(Endpoint, '?', Prefix)
		),
		atom_concat(Prefix, Query, URL).

	atom_list([]).
	atom_list([Atom| Atoms]) :-
		atom(Atom),
		atom_list(Atoms).

	valid_client_authentication(none).
	valid_client_authentication(client_secret_basic(Secret)) :-
		atom(Secret).
	valid_client_authentication(client_secret_post(Secret)) :-
		atom(Secret).

:- end_category.
