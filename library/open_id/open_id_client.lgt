%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_client,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'HTTP-facing OpenID Connect client predicates using http_socket_process for TLS support.'
	]).

	:- public(discover/3).
	:- mode(discover(+atom, -compound, +list(compound)), one_or_error).
	:- info(discover/3, [
		comment is 'Discovers and validates OpenID Provider metadata for the given issuer.',
		argnames is ['Issuer', 'Provider', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Issuer`` is a variable' - instantiation_error,
			'``Issuer`` is not an absolute or allowed issuer URL' - domain_error(open_id_issuer, 'Issuer'),
			'The discovery endpoint returned a non-success status' - domain_error(open_id_http_status, 'Code'),
			'The discovery endpoint response body is not JSON' - domain_error(open_id_json_response, 'Body'),
			'The discovered issuer does not match ``Issuer``' - domain_error(open_id_issuer, 'DiscoveredIssuer'),
			'A provider metadata URL is not secure' - domain_error(open_id_provider_metadata_url, 'URL')
		]
	]).

	:- public(jwks/3).
	:- mode(jwks(+compound, -term, +list(compound)), one_or_error).
	:- info(jwks/3, [
		comment is 'Fetches the JSON Web Key Set for the given provider metadata.',
		argnames is ['Provider', 'JWKSet', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The JWKS endpoint returned a non-success status' - domain_error(open_id_http_status, 'Code'),
			'The JWKS endpoint response body is not JSON' - domain_error(open_id_json_response, 'Body')
		]
	]).

	:- public(exchange_code/5).
	:- mode(exchange_code(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(exchange_code/5, [
		comment is 'Exchanges an authorization code for token response data.',
		argnames is ['Provider', 'Code', 'Session', 'Tokens', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Session`` is missing required authorization-code exchange data' - domain_error(open_id_session, missing('Session', 'Name')),
			'The token endpoint returned a non-success status' - domain_error(open_id_http_status, 'Code'),
			'The token endpoint response body is not JSON' - domain_error(open_id_json_response, 'Body'),
			'The token endpoint JSON response is missing a required member' - domain_error(open_id_token_response, missing('Name'))
		]
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	:- uses(list, [
		append/2, append/3, member/2
	]).

	discover(Issuer, Provider, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^ensure_secure_url(open_id_issuer, Issuer, MergedOptions),
		open_id_discovery::discovery_url(Issuer, URL),
		get_json(URL, JSON, MergedOptions),
		open_id_discovery::provider(Issuer, JSON, Provider, MergedOptions).

	jwks(Provider, JWKSet, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^provider_property(Provider, jwks_uri, URL),
		get_json(URL, JWKSet, MergedOptions).

	exchange_code(Provider, Code, Session, Tokens, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^provider_property(Provider, token_endpoint, URL),
		session_property(Session, client_id, ClientId),
		session_property(Session, redirect_uri, RedirectURI),
		session_property(Session, code_verifier, Verifier),
		token_request_parameters(Code, ClientId, RedirectURI, Verifier, MergedOptions, Parameters),
		post_form_json(URL, Parameters, JSON, MergedOptions),
		tokens(JSON, Tokens).

	get_json(URL, JSON, Options) :-
		http_options(URL, Options, HTTPOptions),
		http_client(http_socket_process)::get(URL, Response, HTTPOptions),
		response_json(Response, JSON).

	post_form_json(URL, Parameters, JSON, Options) :-
		http_options(URL, Options, HTTPOptions),
		Body = content('application/x-www-form-urlencoded', form(Parameters)),
		http_client(http_socket_process)::post(URL, Body, Response, HTTPOptions),
		response_json(Response, JSON).

	response_json(Response, JSON) :-
		status(Response, status(Code, _)),
		(	Code >= 200,
			Code =< 299 ->
			true
		;	domain_error(open_id_http_status, Code)
		),
		body(Response, Body),
		(	Body = content(_, json(JSON)) ->
			true
		;	domain_error(open_id_json_response, Body)
		).

	token_request_parameters(Code, ClientId, RedirectURI, Verifier, Options, Parameters) :-
		Base = [
			grant_type-'authorization_code',
			code-Code,
			redirect_uri-RedirectURI,
			client_id-ClientId,
			code_verifier-Verifier
		],
		(	^^option(client_secret_post(Secret), Options) ->
			append(Base, [client_secret-Secret], Parameters)
		;	Parameters = Base
		).

	tokens(JSON, tokens(Properties)) :-
		required_json_member(access_token, JSON, AccessToken),
		required_json_member(token_type, JSON, TokenType),
		optional_json_property(id_token, JSON, IdTokenPairs),
		optional_json_property(expires_in, JSON, ExpiresInPairs),
		optional_json_property(refresh_token, JSON, RefreshTokenPairs),
		optional_json_property(scope, JSON, ScopePairs),
		append([
			[access_token(AccessToken), token_type(TokenType)],
			IdTokenPairs, ExpiresInPairs, RefreshTokenPairs, ScopePairs,
			[raw(JSON)]
		], Properties).

	required_json_member(Name, JSON, Value) :-
		(	^^json_member(Name, JSON, Value) ->
			true
		;	domain_error(open_id_token_response, missing(Name))
		).

	optional_json_property(Name, JSON, [Term]) :-
		^^json_member(Name, JSON, Value),
		!,
		Term =.. [Name, Value].
	optional_json_property(_, _, []).

	session_property(session(Properties), Name, Value) :-
		Term =.. [Name, Value],
		^^option(Term, Properties),
		!.
	session_property(Session, Name, _) :-
		domain_error(open_id_session, missing(Session, Name)).

	http_options(URL, Options, HTTPOptions) :-
		http_options_list(Options, HTTPOptions0),
		tls_connection_options(URL, Options, ConnectionOptions),
		(	ConnectionOptions == none ->
			HTTPOptions = HTTPOptions0
		;	HTTPOptions = [connection_options(ConnectionOptions)| HTTPOptions0]
		).

	http_options_list([], []).
	http_options_list([Option| Options], HTTPOptions) :-
		(	http_option(Option) ->
			HTTPOptions = [Option| HTTPOptionsTail],
			http_options_list(Options, HTTPOptionsTail)
		;	open_id_option(Option) ->
			http_options_list(Options, HTTPOptions)
		;	domain_error(open_id_client_option, Option)
		).

	http_option(headers(_)).
	http_option(query(_)).
	http_option(version(_)).
	http_option(properties(_)).

	open_id_option(allow_insecure_http(_)).
	open_id_option(client_secret_post(_)).
	open_id_option(clock_skew(_)).
	open_id_option(now(_)).
	open_id_option(expected_audience(_)).
	open_id_option(expected_nonce(_)).
	open_id_option(client_id(_)).
	open_id_option(required_claims(_)).
	open_id_option(allow_algorithms(_)).
	open_id_option(code_verifier(_)).
	open_id_option(openssl_executable(_)).
	open_id_option(openssl_arguments(_)).
	open_id_option(server_name(_)).
	open_id_option(connection_options(_)).

	tls_connection_options(URL, Options, ConnectionOptions) :-
		(	^^https_url(URL) ->
			^^option(connection_options(Existing), Options, connection_options([])),
			tls_options(Options, TLSOptions),
			append_tls_transport(Existing, ExistingTLS),
			append(ExistingTLS, TLSOptions, ConnectionOptions)
		;	^^option(connection_options(ConnectionOptions), Options) ->
			true
		;	ConnectionOptions = none
		).

	append_tls_transport(Options, Options) :-
		member(connection_transport(_), Options),
		!.
	append_tls_transport(Options, [connection_transport(tls)| Options]).

	tls_options([], []).
	tls_options([Option| Options], TLSOptions) :-
		(	tls_option(Option),
			\+ connection_option_present(Option, Options) ->
			TLSOptions = [Option| TLSOptionsTail]
		;	TLSOptions = TLSOptionsTail
		),
		tls_options(Options, TLSOptionsTail).

	tls_option(openssl_executable(_)).
	tls_option(openssl_arguments(_)).
	tls_option(server_name(_)).

	connection_option_present(Option, Options) :-
		functor(Option, Name, _),
		member(connection_options(ConnectionOptions), Options),
		member(ConnectionOption, ConnectionOptions),
		functor(ConnectionOption, Name, _),
		!.

:- end_object.
