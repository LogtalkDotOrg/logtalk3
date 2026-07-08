%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_jwks,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'JSON Web Key Set parsing and key selection helpers.'
	]).

	:- public(select_key/3).
	:- mode(select_key(+term, +term, -term), one_or_error).
	:- info(select_key/3, [
		comment is 'Selects a matching verification key from a JWK Set for a JWT header.',
		argnames is ['JWKSet', 'Header', 'Key'],
		exceptions is [
			'``JWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(open_id_jwks, 'JWKSet'),
			'``Header`` does not contain a valid ``alg`` member' - domain_error(open_id_jwt_header, 'Header'),
			'No compatible key exists for ``Header``' - existence_error(open_id_jwk, 'Header')
		]
	]).

	:- public(header_algorithm/2).
	:- mode(header_algorithm(+term, -atom), one_or_error).
	:- info(header_algorithm/2, [
		comment is 'Extracts and validates the JWT header algorithm.',
		argnames is ['Header', 'Algorithm'],
		exceptions is [
			'``Header`` does not contain a valid ``alg`` member' - domain_error(open_id_jwt_header, 'Header')
		]
	]).

	:- public(header_key_id/2).
	:- mode(header_key_id(+term, -atom), zero_or_one).
	:- info(header_key_id/2, [
		comment is 'Extracts the optional key identifier from a JWT header.',
		argnames is ['Header', 'KeyId']
	]).

	:- uses(list, [
		member/2
	]).

	select_key(JWKSet, Header, Key) :-
		(	^^json_member(keys, JWKSet, Keys),
			list::valid(Keys) ->
			true
		;	domain_error(open_id_jwks, JWKSet)
		),
		header_algorithm(Header, Algorithm),
		(	header_key_id(Header, KeyId) ->
			true
		;	KeyId = none
		),
		select_matching_key(Keys, Algorithm, KeyId, Key),
		!.
	select_key(_JWKSet, Header, _Key) :-
		existence_error(open_id_jwk, Header).

	header_algorithm(Header, Algorithm) :-
		(	^^json_member(alg, Header, Algorithm),
			atom(Algorithm) ->
			true
		;	domain_error(open_id_jwt_header, Header)
		).

	header_key_id(Header, KeyId) :-
		^^json_member(kid, Header, KeyId),
		atom(KeyId).

	select_matching_key([Key| _], Algorithm, KeyId, Key) :-
		matching_key(Key, Algorithm, KeyId),
		!.
	select_matching_key([_| Keys], Algorithm, KeyId, Key) :-
		select_matching_key(Keys, Algorithm, KeyId, Key).

	matching_key(Key, 'RS256', KeyId) :-
		required_key_member(kty, Key, 'RSA'),
		matching_key_id(Key, KeyId),
		matching_key_algorithm(Key, 'RS256'),
		matching_key_use(Key).
	matching_key(Key, 'ES256', KeyId) :-
		required_key_member(kty, Key, 'EC'),
		required_key_member(crv, Key, 'P-256'),
		matching_key_id(Key, KeyId),
		matching_key_algorithm(Key, 'ES256'),
		matching_key_use(Key).

	required_key_member(Name, Key, Value) :-
		^^json_member(Name, Key, Value).

	matching_key_id(_Key, none) :-
		!.
	matching_key_id(Key, KeyId) :-
		^^json_member(kid, Key, KeyId).

	matching_key_algorithm(Key, Algorithm) :-
		(	^^json_member(alg, Key, KeyAlgorithm) ->
			KeyAlgorithm == Algorithm
		;	true
		).

	matching_key_use(Key) :-
		(	^^json_member(use, Key, Use) ->
			Use == sig
		;	^^json_member(key_ops, Key, KeyOps) ->
			member(verify, KeyOps)
		;	true
		).

:- end_object.
