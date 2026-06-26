%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_jws,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'JWS signing input construction and signature algorithm dispatch.'
	]).

	:- public(verify/5).
	:- mode(verify(+atom, +atom, +list(byte), +term, +list(compound)), zero_or_one_or_error).
	:- info(verify/5, [
		comment is 'Verifies a JWS signature for a signing input, algorithm, key, and options.',
		argnames is ['Algorithm', 'SigningInput', 'Signature', 'Key', 'Options'],
		exceptions is [
			'``Algorithm`` is unsupported' - domain_error(jwt_algorithm, 'Algorithm'),
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

	verify(Algorithm, SigningInput, Signature, Key, Options) :-
		(	Algorithm == none ->
			^^option(allow_none(true), Options),
			Signature == []
		;	jwt_jwa::hmac_hash(Algorithm, Hash) ->
			jwt_jwk::symmetric_key_bytes(Key, KeyBytes),
			atom_codes(SigningInput, MessageBytes),
			hmac::digest(Hash, KeyBytes, MessageBytes, ExpectedSignature),
			ExpectedSignature == Signature
		;	(Algorithm == 'RS256'; Algorithm == 'ES256') ->
			jwt_jwk::public_key_pem(Key, PEM),
			jwt_openssl::verify(Algorithm, PEM, SigningInput, Signature, Options)
		;	domain_error(jwt_algorithm, Algorithm)
		).

	sign(Header, Claims, Key, Token, Options) :-
		sign_payload(Header, Claims, Key, Token, Options).

	sign_payload(Header, Payload, Key, Token, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		jwt_jwa::header_algorithm(Header, Algorithm),
		jwt_jwa::allowed_algorithm(Algorithm, MergedOptions),
		jwt_compact::signing_input(Header, Payload, _HeaderSegment, SigningInput),
		signature(Algorithm, SigningInput, Key, Signature, MergedOptions),
		jwt_compact::compact(_HeaderSegment, SigningInput, Signature, Token).

	signature(none, _SigningInput, _Key, [], Options) :-
		^^option(allow_none(true), Options),
		!.
	signature(Algorithm, SigningInput, Key, Signature, _Options) :-
		jwt_jwa::hmac_hash(Algorithm, Hash),
		!,
		jwt_jwk::symmetric_key_bytes(Key, KeyBytes),
		atom_codes(SigningInput, MessageBytes),
		hmac::digest(Hash, KeyBytes, MessageBytes, Signature).
	signature(Algorithm, _SigningInput, _Key, _Signature, _Options) :-
		domain_error(jwt_signing_algorithm, Algorithm).

:- end_object.
