%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Facade predicates for parsing, signing, verifying, and validating JSON Web Tokens.'
	]).

	:- public(decode/3).
	:- mode(decode(+atom, -term, -term), one_or_error).
	:- info(decode/3, [
		comment is 'Decodes a compact JWT into header and claims JSON terms without verifying the signature.',
		argnames is ['Token', 'Header', 'Claims'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(decode/5).
	:- mode(decode(+atom, -term, -term, -list(byte), -atom), one_or_error).
	:- info(decode/5, [
		comment is 'Decodes a compact JWT into header, claims, signature bytes, and signing input.',
		argnames is ['Token', 'Header', 'Claims', 'Signature', 'SigningInput'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(header/2).
	:- mode(header(+atom, -term), one_or_error).
	:- info(header/2, [
		comment is 'Decodes only the JWT header JSON term.',
		argnames is ['Token', 'Header'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(claims/2).
	:- mode(claims(+atom, -term), one_or_error).
	:- info(claims/2, [
		comment is 'Decodes only the JWT claims JSON term.',
		argnames is ['Token', 'Claims'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(claim/3).
	:- mode(claim(+term, +atom, -term), zero_or_one).
	:- info(claim/3, [
		comment is 'Looks up a claim value by name in a claims JSON term.',
		argnames is ['Claims', 'Name', 'Value']
	]).

	:- public(peek_algorithm/2).
	:- mode(peek_algorithm(+atom, -atom), one_or_error).
	:- info(peek_algorithm/2, [
		comment is 'Decodes the JWT header and returns its algorithm.',
		argnames is ['Token', 'Algorithm'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64),
			'The decoded JWT header does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header')
		]
	]).

	:- public(peek_key_id/2).
	:- mode(peek_key_id(+atom, -atom), zero_or_one).
	:- info(peek_key_id/2, [
		comment is 'Decodes the JWT header and returns its optional key identifier.',
		argnames is ['Token', 'KeyId']
	]).

	:- public(verify/4).
	:- mode(verify(+atom, +term, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify/4, [
		comment is 'Verifies a compact JWT using a key or JWK Set and returns the claims.',
		argnames is ['Token', 'KeyOrJWKSet', 'Claims', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64),
			'The decoded JWT header does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'``KeyOrJWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(jwt_jwks, 'KeyOrJWKSet'),
			'No compatible key exists for the JWT header' - existence_error(jwt_jwk, 'Header'),
			'``KeyOrJWKSet`` is not a valid symmetric key' - domain_error(jwt_symmetric_key, 'KeyOrJWKSet'),
			'``KeyOrJWKSet`` is not a supported public JWK' - domain_error(jwt_jwk_public_key, 'KeyOrJWKSet'),
			'The ES256 signature is not 64 bytes long' - domain_error(jwt_es256_signature, 'Signature'),
			'The OpenSSL executable does not exist' - existence_error(os_command, 'Executable'),
			'A required claim is missing' - domain_error(jwt_claims, missing('Name')),
			'A claim has an invalid value' - domain_error(jwt_claim('Name'), 'Value'),
			'A time claim value is not a number' - type_error(time_number, 'Name'-'Time')
		]
	]).

	:- public(verify/5).
	:- mode(verify(+atom, -term, -term, +term, +list(compound)), zero_or_one_or_error).
	:- info(verify/5, [
		comment is 'Verifies a compact JWT using a key or JWK Set and returns the header and claims.',
		argnames is ['Token', 'Header', 'Claims', 'KeyOrJWKSet', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is an atom but not a valid compact JWT' - domain_error(jwt_compact_serialization, 'Token'),
			'``Token`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64),
			'The decoded JWT header does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'``KeyOrJWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(jwt_jwks, 'KeyOrJWKSet'),
			'No compatible key exists for the JWT header' - existence_error(jwt_jwk, 'Header'),
			'``KeyOrJWKSet`` is not a valid symmetric key' - domain_error(jwt_symmetric_key, 'KeyOrJWKSet'),
			'``KeyOrJWKSet`` is not a supported public JWK' - domain_error(jwt_jwk_public_key, 'KeyOrJWKSet'),
			'The ES256 signature is not 64 bytes long' - domain_error(jwt_es256_signature, 'Signature'),
			'The OpenSSL executable does not exist' - existence_error(os_command, 'Executable'),
			'A required claim is missing' - domain_error(jwt_claims, missing('Name')),
			'A claim has an invalid value' - domain_error(jwt_claim('Name'), 'Value'),
			'A time claim value is not a number' - type_error(time_number, 'Name'-'Time')
		]
	]).

	:- public(verify_signature/5).
	:- mode(verify_signature(+atom, +atom, +list(byte), +term, +list(compound)), zero_or_one_or_error).
	:- info(verify_signature/5, [
		comment is 'Verifies a JWS signature for a signing input, algorithm, key, and options.',
		argnames is ['Algorithm', 'SigningInput', 'Signature', 'Key', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Algorithm`` is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'``Key`` is not a valid symmetric key' - domain_error(jwt_symmetric_key, 'Key'),
			'``Key`` is not a supported public JWK' - domain_error(jwt_jwk_public_key, 'Key'),
			'``Signature`` is not a 64-byte raw ES256 signature' - domain_error(jwt_es256_signature, 'Signature'),
			'The OpenSSL executable does not exist' - existence_error(os_command, 'Executable')
		]
	]).

	:- public(sign/5).
	:- mode(sign(+term, +term, +term, -atom, +list(compound)), one_or_error).
	:- info(sign/5, [
		comment is 'Signs claims as a compact JWT using the given header, key, and options.',
		argnames is ['Header', 'Claims', 'Key', 'Token', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Header`` does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'``Header`` or ``Claims`` cannot be encoded as JSON' - domain_error(json_sink, 'HeaderOrClaims'),
			'``Key`` is not a valid symmetric key' - domain_error(jwt_symmetric_key, 'Key'),
			'``Algorithm`` is not a supported signing algorithm' - domain_error(jwt_signing_algorithm, 'Algorithm')
		]
	]).

	:- public(sign_payload/5).
	:- mode(sign_payload(+term, +term, +term, -atom, +list(compound)), one_or_error).
	:- info(sign_payload/5, [
		comment is 'Signs a payload JSON term as a compact JWS using the given header, key, and options.',
		argnames is ['Header', 'Payload', 'Key', 'Token', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Header`` does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'``Header`` or ``Payload`` cannot be encoded as JSON' - domain_error(json_sink, 'HeaderOrPayload'),
			'``Key`` is not a valid symmetric key' - domain_error(jwt_symmetric_key, 'Key'),
			'``Algorithm`` is not a supported signing algorithm' - domain_error(jwt_signing_algorithm, 'Algorithm')
		]
	]).

	:- public(validate_claims/3).
	:- mode(validate_claims(+term, +list(compound), +list(compound)), one_or_error).
	:- info(validate_claims/3, [
		comment is 'Validates JWT claims using registered-claim defaults, a policy list, and options.',
		argnames is ['Claims', 'Policy', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'A required claim is missing' - domain_error(jwt_claims, missing('Name')),
			'A claim has an invalid value' - domain_error(jwt_claim('Name'), 'Value'),
			'``Policy`` contains an invalid claim policy' - domain_error(jwt_claim_policy, 'Policy'),
			'A time claim value is not a number' - type_error(time_number, 'Name'-'Time'),
			'``Policy`` contains an invalid time-claim kind' - domain_error(jwt_time_claim_kind, 'Kind')
		]
	]).

	:- public(validate_claim/3).
	:- mode(validate_claim(+term, +compound, +list(compound)), one_or_error).
	:- info(validate_claim/3, [
		comment is 'Validates a single JWT claim policy against a claims JSON term.',
		argnames is ['Claims', 'ClaimPolicy', 'Options'],
		exceptions is [
			'A required claim is missing' - domain_error(jwt_claims, missing('Name')),
			'A claim has an invalid value' - domain_error(jwt_claim('Name'), 'Value'),
			'``ClaimPolicy`` is not a valid claim policy' - domain_error(jwt_claim_policy, 'ClaimPolicy'),
			'A time claim value is not a number' - type_error(time_number, 'Name'-'Time'),
			'``ClaimPolicy`` contains an invalid time-claim kind' - domain_error(jwt_time_claim_kind, 'Kind')
		]
	]).

	decode(Token, Header, Claims) :-
		jwt_compact::decode(Token, Header, Claims).

	decode(Token, Header, Claims, Signature, SigningInput) :-
		jwt_compact::decode(Token, Header, Claims, Signature, SigningInput).

	header(Token, Header) :-
		jwt_compact::header(Token, Header).

	claims(Token, Claims) :-
		jwt_compact::claims(Token, Claims).

	claim(Claims, Name, Value) :-
		jwt_claims::claim(Claims, Name, Value).

	peek_algorithm(Token, Algorithm) :-
		header(Token, Header),
		jwt_jwa::header_algorithm(Header, Algorithm).

	peek_key_id(Token, KeyId) :-
		header(Token, Header),
		jwt_jwa::header_key_id(Header, KeyId).

	verify(Token, KeyOrJWKSet, Claims, Options) :-
		verify(Token, _Header, Claims, KeyOrJWKSet, Options).

	verify(Token, Header, Claims, KeyOrJWKSet, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		decode(Token, Header, Claims, Signature, SigningInput),
		jwt_jwa::header_algorithm(Header, Algorithm),
		jwt_jwa::allowed_algorithm(Algorithm, MergedOptions),
		verification_key(KeyOrJWKSet, Header, Key, MergedOptions),
		verify_signature(Algorithm, SigningInput, Signature, Key, MergedOptions),
		^^option(claim_policy(Policy), MergedOptions),
		jwt_claims::validate_claims(Claims, Policy, MergedOptions).

	verify_signature(Algorithm, SigningInput, Signature, Key, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		jwt_jwa::allowed_algorithm(Algorithm, MergedOptions),
		jwt_jws::verify(Algorithm, SigningInput, Signature, Key, MergedOptions).

	sign(HeaderOptions, Claims, Key, Token, Options) :-
		jwt_jws::sign(HeaderOptions, Claims, Key, Token, Options).

	sign_payload(HeaderOptions, Payload, Key, Token, Options) :-
		jwt_jws::sign_payload(HeaderOptions, Payload, Key, Token, Options).

	validate_claims(Claims, Policy, Options) :-
		jwt_claims::validate_claims(Claims, Policy, Options).

	validate_claim(Claims, ClaimPolicy, Options) :-
		jwt_claims::validate_claim(Claims, ClaimPolicy, Options).

	verification_key(KeyOrJWKSet, Header, Key, Options) :-
		(	^^json_member(keys, KeyOrJWKSet, _) ->
			jwt_jwks::select_key(KeyOrJWKSet, Header, Key, Options)
		;	Key = KeyOrJWKSet
		).

:- end_object.
