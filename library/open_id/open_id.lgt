%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-27,
		comment is 'Facade predicates for a portable OpenID Connect Authorization Code + PKCE client.'
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

	:- public(authorization_url/5).
	:- mode(authorization_url(+compound, +compound, -atom, -compound, +list(compound)), one_or_error).
	:- info(authorization_url/5, [
		comment is 'Builds an authorization URL and returns the session data needed for the code exchange and ID-token validation.',
		argnames is ['Provider', 'Request', 'URL', 'Session', 'Options'],
		exceptions is [
			'``Request`` is neither a direct options list nor a request wrapper term containing an options list' - domain_error(open_id_options, 'Request'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Request`` is missing a required authorization request option' - domain_error(open_id_authorization_request, missing('Name')),
			'A request value that must be an atom is not an atom' - type_error(atom, 'Value'),
			'A scope value is neither an atom nor a list of atoms' - domain_error(open_id_space_separated_atom, 'Value'),
			'The PKCE code verifier is a variable' - instantiation_error,
			'The PKCE code verifier is not valid' - domain_error(open_id_code_verifier, 'Verifier')
		]
	]).

	:- public(authorization_response/3).
	:- mode(authorization_response(+atom, -compound, +list(compound)), one_or_error).
	:- info(authorization_response/3, [
		comment is 'Parses an authorization callback URL into either an authorization response or an authorization error term.',
		argnames is ['CallbackURL', 'Response', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``CallbackURL`` is not a valid callback URL or relative reference' - domain_error(open_id_authorization_response, 'CallbackURL')
		]
	]).

	:- public(authorization_code/4).
	:- mode(authorization_code(+atom, +compound, -atom, +list(compound)), one_or_error).
	:- info(authorization_code/4, [
		comment is 'Parses an authorization callback URL, validates the session state, and returns the authorization code.',
		argnames is ['CallbackURL', 'Session', 'Code', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Session`` is missing required state data' - domain_error(open_id_session, missing('Session', state)),
			'The callback response is an authorization error' - domain_error(open_id_authorization_response, authorization_error('Response')),
			'The callback response state does not match the session state' - domain_error(open_id_authorization_response, state_mismatch('Expected', 'Actual'))
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

	:- public(logout_url/4).
	:- mode(logout_url(+compound, +compound, -atom, +list(compound)), one_or_error).
	:- info(logout_url/4, [
		comment is 'Builds a logout URL for an RP-initiated logout request.',
		argnames is ['Provider', 'Request', 'URL', 'Options'],
		exceptions is [
			'``Request`` is neither a direct options list nor a request wrapper term containing an options list' - domain_error(open_id_options, 'Request'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The provider metadata is missing an end-session endpoint' - domain_error(open_id_provider, missing(end_session_endpoint)),
			'A request value that must be an atom is not an atom' - type_error(atom, 'Value'),
			'A ``ui_locales`` value is neither an atom nor a list of atoms' - domain_error(open_id_space_separated_atom, 'Value'),
			'The ``post_logout_redirect_uri`` is not secure' - domain_error(open_id_post_logout_redirect_uri, 'URL')
		]
	]).

	:- public(jwks/3).
	:- mode(jwks(+compound, -term, +list(compound)), one_or_error).
	:- info(jwks/3, [
		comment is 'Fetches the JSON Web Key Set for the provider.',
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

	:- public(cached_jwks/3).
	:- mode(cached_jwks(+compound, -term, +list(compound)), one_or_error).
	:- info(cached_jwks/3, [
		comment is 'Returns a cached JWKS for a provider, refreshing it when needed.',
		argnames is ['Provider', 'JWKSet', 'Options']
	]).

	:- public(verify_id_token/4).
	:- mode(verify_id_token(+atom, +compound, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify_id_token/4, [
		comment is 'Verifies an ID-token using cached JWKS data, refreshing the JWKS once when the token key identifier is unknown.',
		argnames is ['Token', 'Provider', 'Claims', 'Options']
	]).

	:- public(verify_id_token/5).
	:- mode(verify_id_token(+atom, +compound, +term, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify_id_token/5, [
		comment is 'Verifies an ID-token signature and claims using the given provider metadata and JWKS.',
		argnames is ['Token', 'Provider', 'JWKSet', 'Claims', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64),
			'The JWT header does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(open_id_jwt_algorithm, 'Algorithm'),
			'``JWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(jwt_jwks, 'JWKSet'),
			'No compatible key exists for the JWT header' - existence_error(jwt_jwk, 'Header'),
			'The selected JWK is not a supported public key' - domain_error(jwt_jwk_public_key, 'Key'),
			'The ES256 signature is not 64 bytes long' - domain_error(jwt_es256_signature, 'Signature'),
			'The OpenSSL executable does not exist' - existence_error(os_command, 'Executable'),
			'The ``iss`` claim does not match the provider issuer' - domain_error(open_id_claim(iss), 'Value'),
			'A required OpenID ID-token claim is missing' - domain_error(open_id_claims, missing('Name')),
			'The expected audience option is missing' - domain_error(open_id_claims, missing(expected_audience)),
			'An OpenID ID-token claim has an invalid value' - domain_error(open_id_claim('Name'), 'Value'),
			'An OpenID ID-token time claim value is not a number' - type_error(time_number, 'Name'-'Time')
		]
	]).

	discover(Issuer, Provider, Options) :-
		open_id_client::discover(Issuer, Provider, Options).

	authorization_url(Provider, Request, URL, Session, Options) :-
		open_id_pkce::authorization_url(Provider, Request, URL, Session, Options).

	authorization_response(CallbackURL, Response, Options) :-
		open_id_response::authorization_response(CallbackURL, Response, Options).

	authorization_code(CallbackURL, Session, Code, Options) :-
		open_id_response::authorization_code(CallbackURL, Session, Code, Options).

	exchange_code(Provider, Code, Session, Tokens, Options) :-
		open_id_client::exchange_code(Provider, Code, Session, Tokens, Options).

	refresh_token(Provider, RefreshToken, Tokens, Options) :-
		open_id_client::refresh_token(Provider, RefreshToken, Tokens, Options).

	userinfo(Provider, AccessToken, Claims, Options) :-
		open_id_client::userinfo(Provider, AccessToken, Claims, Options).

	logout_url(Provider, Request, URL, Options) :-
		open_id_logout::logout_url(Provider, Request, URL, Options).

	jwks(Provider, JWKSet, Options) :-
		open_id_client::jwks(Provider, JWKSet, Options).

	cached_jwks(Provider, JWKSet, Options) :-
		open_id_jwks_cache::cached_jwks(Provider, JWKSet, Options).

	verify_id_token(Token, Provider, Claims, Options) :-
		open_id_jwks_cache::verify_id_token(Token, Provider, Claims, Options).

	verify_id_token(Token, Provider, JWKSet, Claims, Options) :-
		open_id_jwt::verify_id_token(Token, Provider, JWKSet, Claims, Options).

:- end_object.
