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
		date is 2026-07-08,
		comment is 'HTTP-facing OpenID Connect client predicates using http_process_transport for TLS support.'
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

	:- public(refresh_token/4).
	:- mode(refresh_token(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(refresh_token/4, [
		comment is 'Exchanges a refresh token for token response data.',
		argnames is ['Provider', 'RefreshToken', 'Tokens', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The ``client_id`` option is missing' - domain_error(open_id_refresh_token, missing(client_id)),
			'The token endpoint returned a non-success status' - domain_error(open_id_http_status, 'Code'),
			'The token endpoint response body is not JSON' - domain_error(open_id_json_response, 'Body'),
			'The token endpoint JSON response is missing a required member' - domain_error(open_id_token_response, missing('Name'))
		]
	]).

	:- public(userinfo/4).
	:- mode(userinfo(+compound, +atom, -term, +list(compound)), one_or_error).
	:- info(userinfo/4, [
		comment is 'Fetches UserInfo claims using the given access token.',
		argnames is ['Provider', 'AccessToken', 'Claims', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The provider metadata is missing a UserInfo endpoint' - domain_error(open_id_provider, missing(userinfo_endpoint)),
			'The UserInfo endpoint returned a non-success status' - domain_error(open_id_http_status, 'Code'),
			'The UserInfo endpoint response body is not JSON' - domain_error(open_id_json_response, 'Body')
		]
	]).

	:- uses(base64, [
		generate/2
	]).

	:- uses(http_core, [
		body/2, encode_body/4, generate_body/3, status/2
	]).

	:- uses(list, [
		append/2, append/3, member/2
	]).

	:- uses(user, [
		atomic_list_concat/2
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
		authorization_code_parameters(Code, RedirectURI, Verifier, ClientId, MergedOptions, Parameters),
		post_form_json(URL, Parameters, ClientId, JSON, MergedOptions),
		tokens(JSON, Tokens).

	refresh_token(Provider, RefreshToken, Tokens, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^provider_property(Provider, token_endpoint, URL),
		required_option(client_id, MergedOptions, ClientId, open_id_refresh_token),
		refresh_token_parameters(RefreshToken, ClientId, MergedOptions, Parameters),
		post_form_json(URL, Parameters, ClientId, JSON, MergedOptions),
		tokens(JSON, Tokens).

	userinfo(Provider, AccessToken, Claims, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		provider_required_property(Provider, userinfo_endpoint, URL),
		userinfo_http_options(URL, AccessToken, MergedOptions, HTTPOptions),
		http_client(http_process_transport)::get(URL, Response, HTTPOptions),
		response_json(Response, Claims).

	get_json(URL, JSON, Options) :-
		http_options(URL, Options, HTTPOptions),
		http_client(http_process_transport)::get(URL, Response, HTTPOptions),
		response_json(Response, JSON).

	post_form_json(URL, Parameters, ClientId, JSON, Options) :-
		token_http_options(URL, ClientId, Options, HTTPOptions),
		Body = content('application/x-www-form-urlencoded', form(Parameters)),
		http_client(http_process_transport)::post(URL, Body, Response, HTTPOptions),
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

	authorization_code_parameters(Code, RedirectURI, Verifier, ClientId, Options, Parameters) :-
		Base = [
			grant_type-'authorization_code',
			code-Code,
			redirect_uri-RedirectURI,
			code_verifier-Verifier
		],
		client_authentication_parameters(ClientId, Options, AuthenticationParameters),
		append(Base, AuthenticationParameters, Parameters).

	refresh_token_parameters(RefreshToken, ClientId, Options, Parameters) :-
		Base = [
			grant_type-'refresh_token',
			refresh_token-RefreshToken
		],
		client_authentication_parameters(ClientId, Options, AuthenticationParameters),
		optional_scope_parameter(Options, ScopeParameters),
		append([Base, AuthenticationParameters, ScopeParameters], Parameters).

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

	provider_required_property(Provider, Name, Value) :-
		(	^^provider_property(Provider, Name, Value) ->
			true
		;	domain_error(open_id_provider, missing(Name))
		).

	required_option(Name, Options, Value, Domain) :-
		Term =.. [Name, Value],
		(	^^option(Term, Options) ->
			true
		;	domain_error(Domain, missing(Name))
		).

	client_authentication_parameters(ClientId, Options, Parameters) :-
		^^option(client_authentication(Authentication), Options),
		!,
		client_authentication_parameters_(Authentication, ClientId, Parameters).

	client_authentication_parameters_(none, ClientId, [client_id-ClientId]).
	client_authentication_parameters_(client_secret_post(Secret), ClientId, [client_id-ClientId, client_secret-Secret]).
	client_authentication_parameters_(client_secret_basic(_Secret), _ClientId, []).

	optional_scope_parameter(Options, [scope-Scope]) :-
		^^option(scope(Scope0), Options),
		!,
		^^space_atom(Scope0, Scope).
	optional_scope_parameter(_Options, []).

	token_http_options(URL, ClientId, Options, HTTPOptions) :-
		http_options(URL, Options, HTTPOptions0),
		client_authentication_headers(ClientId, Options, Headers),
		merge_required_headers(Headers, HTTPOptions0, HTTPOptions).

	userinfo_http_options(URL, AccessToken, Options, HTTPOptions) :-
		http_options(URL, Options, HTTPOptions0),
		bearer_authorization_header(AccessToken, Authorization),
		merge_required_headers([authorization-Authorization, accept-'application/json'], HTTPOptions0, HTTPOptions).

	client_authentication_headers(ClientId, Options, [authorization-Authorization]) :-
		^^option(client_authentication(client_secret_basic(Secret)), Options),
		!,
		basic_authorization_header(ClientId, Secret, Authorization).
	client_authentication_headers(_ClientId, _Options, []).

	basic_authorization_header(ClientId, Secret, Authorization) :-
		form_urlencoded_value(ClientId, EncodedClientId),
		form_urlencoded_value(Secret, EncodedSecret),
		atomic_list_concat([EncodedClientId, ':', EncodedSecret], Credentials),
		atom_codes(Credentials, Codes),
		generate(atom(Base64), Codes),
		atomic_list_concat(['Basic ', Base64], Authorization).

	bearer_authorization_header(AccessToken, Authorization) :-
		atomic_list_concat(['Bearer ', AccessToken], Authorization).

	form_urlencoded_value(Value, EncodedValue) :-
		encode_body('application/x-www-form-urlencoded', [value-Value], [], Body),
		generate_body(atom(Query), Body, []),
		atom_concat('value=', EncodedValue, Query).

	merge_required_headers([], HTTPOptions, HTTPOptions) :-
		!.
	merge_required_headers(Headers, [], [headers(Headers)]) :-
		!.
	merge_required_headers(Headers, [headers(ExistingHeaders)| HTTPOptions], [headers(MergedHeaders)| HTTPOptions]) :-
		!,
		merge_header_lists(Headers, ExistingHeaders, MergedHeaders).
	merge_required_headers(Headers, [HTTPOption| HTTPOptions0], [HTTPOption| HTTPOptions]) :-
		merge_required_headers(Headers, HTTPOptions0, HTTPOptions).

	merge_header_lists([], ExistingHeaders, ExistingHeaders).
	merge_header_lists([Header| Headers], ExistingHeaders0, [Header| ExistingHeaders]) :-
		Header = Name-_,
		remove_header(Name, ExistingHeaders0, ExistingHeaders1),
		merge_header_lists(Headers, ExistingHeaders1, ExistingHeaders).

	remove_header(_Name, [], []).
	remove_header(Name, [Name-_| Headers], RemainingHeaders) :-
		!,
		remove_header(Name, Headers, RemainingHeaders).
	remove_header(Name, [Header| Headers], [Header| RemainingHeaders]) :-
		remove_header(Name, Headers, RemainingHeaders).

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
	open_id_option(client_authentication(_)).
	open_id_option(clock_skew(_)).
	open_id_option(now(_)).
	open_id_option(expected_audience(_)).
	open_id_option(expected_nonce(_)).
	open_id_option(client_id(_)).
	open_id_option(required_claims(_)).
	open_id_option(allow_algorithms(_)).
	open_id_option(code_verifier(_)).
	open_id_option(jwks_cache_ttl(_)).
	open_id_option(openssl_executable(_)).
	open_id_option(openssl_arguments(_)).
	open_id_option(refresh_on_unknown_kid(_)).
	open_id_option(scope(_)).
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
