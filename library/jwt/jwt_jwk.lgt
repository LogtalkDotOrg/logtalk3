%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_jwk,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'JSON Web Key normalization and key material helpers.'
	]).

	:- public(public_key_pem/2).
	:- mode(public_key_pem(+term, -atom), one_or_error).
	:- info(public_key_pem/2, [
		comment is 'Converts a supported JWK public key into PEM SubjectPublicKeyInfo text.',
		argnames is ['Key', 'PEM'],
		exceptions is [
			'``Key`` is not a supported RSA or P-256 EC public JWK' - domain_error(jwt_jwk_public_key, 'Key'),
			'``Key`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(symmetric_key_bytes/2).
	:- mode(symmetric_key_bytes(+term, -list(byte)), one_or_error).
	:- info(symmetric_key_bytes/2, [
		comment is 'Extracts symmetric key bytes from an atom, byte wrapper, or octet JWK.',
		argnames is ['Key', 'Bytes'],
		exceptions is [
			'``Key`` is not an atom, byte wrapper, or octet JWK with Base64URL key material' - domain_error(jwt_symmetric_key, 'Key'),
			'``Key`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- public(compatible_key/2).
	:- mode(compatible_key(+term, +atom), zero_or_one).
	:- info(compatible_key/2, [
		comment is 'Succeeds when a key is compatible with the requested JWT algorithm.',
		argnames is ['Key', 'Algorithm']
	]).

	public_key_pem(Key, PEM) :-
		jwt_der::public_key_pem(Key, PEM).

	symmetric_key_bytes(Key, Bytes) :-
		atom(Key),
		!,
		atom_codes(Key, Bytes).
	symmetric_key_bytes(bytes(Bytes), Bytes) :-
		list::valid(Bytes),
		!.
	symmetric_key_bytes(Key, Bytes) :-
		^^json_member(kty, Key, 'oct'),
		^^json_member(k, Key, Encoded),
		atom(Encoded),
		!,
		^^base64url_atom_bytes(Encoded, Bytes).
	symmetric_key_bytes(Key, _) :-
		domain_error(jwt_symmetric_key, Key).

	compatible_key(Key, Algorithm) :-
		jwt_jwa::key_type(Algorithm, KeyType),
		^^json_member(kty, Key, KeyType),
		!.
	compatible_key(Key, Algorithm) :-
		Algorithm == 'HS256',
		(	atom(Key)
		;	Key = bytes(Bytes),
			list::valid(Bytes)
		).

:- end_object.
