%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_jwt,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-27,
		comment is 'JWT parsing and ID-token verification helpers.'
	]).

	:- public(decode/3).
	:- mode(decode(+atom, -term, -term), one_or_error).
	:- info(decode/3, [
		comment is 'Decodes a JWT into its header and claims JSON terms without verifying the signature.',
		argnames is ['Token', 'Header', 'Claims'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
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

	:- uses(list, [
		member/2
	]).

	:- uses(os, [
		time_stamp/1
	]).

	decode(Token, Header, Claims) :-
		jwt::decode(Token, Header, Claims).

	verify_id_token(Token, Provider, JWKSet, Claims, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		jwt::header(Token, Header),
		jwt_jwa::header_algorithm(Header, Algorithm),
		allowed_algorithm(Algorithm, MergedOptions),
		jwt_options(MergedOptions, JWTOptions),
		jwt::verify(Token, _VerifiedHeader, Claims, JWKSet, JWTOptions),
		validate_claims(Claims, Provider, MergedOptions).

	jwt_options(Options, [allow_missing_exp(true)| JWTOptions]) :-
		jwt_options_(Options, JWTOptions).

	jwt_options_([], []).
	jwt_options_([Option| Options], JWTOptions) :-
		(	jwt_option(Option) ->
			JWTOptions = [Option| JWTOptionsTail]
		;	JWTOptions = JWTOptionsTail
		),
		jwt_options_(Options, JWTOptionsTail).

	jwt_option(allow_algorithms(_)).
	jwt_option(clock_skew(_)).
	jwt_option(now(_)).
	jwt_option(openssl_executable(_)).
	jwt_option(openssl_arguments(_)).

	allowed_algorithm(Algorithm, Options) :-
		(	Algorithm == none ->
			domain_error(open_id_jwt_algorithm, Algorithm)
		;	^^option(allow_algorithms(Algorithms), Options) ->
			(	member(Algorithm, Algorithms) ->
				true
			;	domain_error(open_id_jwt_algorithm, Algorithm)
			)
		;	domain_error(open_id_jwt_algorithm, Algorithm)
		).

	validate_claims(Claims, Provider, Options) :-
		^^provider_property(Provider, issuer, Issuer),
		required_claim_value(iss, Claims, Issuer),
		required_subject(Claims),
		expected_audience(Options, Audience),
		required_claim(aud, Claims, AudienceClaim),
		validate_audience(AudienceClaim, Audience),
		validate_authorized_party(AudienceClaim, Claims, Audience),
		validate_nonce(Claims, Options),
		current_time(Options, Now),
		^^option(clock_skew(ClockSkew), Options),
		validate_time_claim(exp, Claims, Now, ClockSkew, expiration),
		validate_optional_time_claim(nbf, Claims, Now, ClockSkew, not_before),
		validate_iat(Claims, Now, ClockSkew),
		validate_required_claims(Claims, Options).

	required_claim_value(Name, Claims, Value) :-
		(	^^json_member(Name, Claims, Value0) ->
			(	Value0 == Value ->
				true
			;	domain_error(open_id_claim(Name), Value0)
			)
		;	domain_error(open_id_claims, missing(Name))
		).

	required_claim(Name, Claims, Value) :-
		(	^^json_member(Name, Claims, Value) ->
			true
		;	domain_error(open_id_claims, missing(Name))
		).

	expected_audience(Options, Audience) :-
		(	^^option(expected_audience(Audience), Options) ->
			true
		;	^^option(client_id(Audience), Options) ->
			true
		;	domain_error(open_id_claims, missing(expected_audience))
		).

	validate_audience(Audience, Expected) :-
		atom(Audience),
		!,
		(	Audience == Expected ->
			true
		;	domain_error(open_id_claim(aud), Audience)
		).
	validate_audience(Audiences, Expected) :-
		list::valid(Audiences),
		member(Expected, Audiences),
		!.
	validate_audience(Audience, _) :-
		domain_error(open_id_claim(aud), Audience).

	validate_authorized_party(Audience, Claims, Expected) :-
		list::valid(Audience),
		Audience = [_,_| _],
		!,
		required_claim_value(azp, Claims, Expected).
	validate_authorized_party(_Audience, Claims, Expected) :-
		(	^^json_member(azp, Claims, AuthorizedParty) ->
			(	AuthorizedParty == Expected ->
				true
			;	domain_error(open_id_claim(azp), AuthorizedParty)
			)
		;	true
		).

	validate_nonce(Claims, Options) :-
		(	^^option(expected_nonce(ExpectedNonce), Options) ->
			required_claim_value(nonce, Claims, ExpectedNonce)
		;	true
		).

	required_subject(Claims) :-
		required_claim(sub, Claims, Subject),
		(	atom(Subject),
			Subject \== '' ->
			true
		;	domain_error(open_id_claim(sub), Subject)
		).

	current_time(Options, Now) :-
		(	^^option(now(Now), Options) ->
			true
		;	time_stamp(Now)
		).

	validate_time_claim(Name, Claims, Now, ClockSkew, Kind) :-
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Kind == expiration ->
			(	Now =< Time + ClockSkew ->
				true
			;	domain_error(open_id_claim(Name), Time)
			)
		;	Kind == not_before ->
			(	Now + ClockSkew >= Time ->
				true
			;	domain_error(open_id_claim(Name), Time)
			)
		;	domain_error(open_id_time_claim_kind, Kind)
		).

	validate_optional_time_claim(Name, Claims, Now, ClockSkew, Kind) :-
		(	^^json_member(Name, Claims, _) ->
			validate_time_claim(Name, Claims, Now, ClockSkew, Kind)
		;	true
		).

	validate_iat(Claims, Now, ClockSkew) :-
		required_claim(iat, Claims, IssuedAt),
		validate_time_number(iat, IssuedAt),
		(	IssuedAt =< Now + ClockSkew ->
			true
		;	domain_error(open_id_claim(iat), IssuedAt)
		).

	validate_time_number(Name, Time) :-
		(	number(Time) ->
			true
		;	type_error(time_number, Name-Time)
		).

	validate_required_claims(Claims, Options) :-
		^^option(required_claims(RequiredClaims), Options),
		validate_required_claims_(RequiredClaims, Claims).

	validate_required_claims_([], _Claims).
	validate_required_claims_([Claim| Claims], JSON) :-
		(	^^json_member(Claim, JSON, _) ->
			validate_required_claims_(Claims, JSON)
		;	domain_error(open_id_claims, missing(Claim))
		).

:- end_object.
