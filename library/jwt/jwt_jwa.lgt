%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_jwa,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'JSON Web Algorithm metadata and header helpers.'
	]).

	:- public(header_algorithm/2).
	:- mode(header_algorithm(+term, -atom), one_or_error).
	:- info(header_algorithm/2, [
		comment is 'Extracts and validates the JWT header algorithm.',
		argnames is ['Header', 'Algorithm'],
		exceptions is [
			'``Header`` does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header')
		]
	]).

	:- public(header_key_id/2).
	:- mode(header_key_id(+term, -atom), zero_or_one).
	:- info(header_key_id/2, [
		comment is 'Extracts the optional key identifier from a JWT header.',
		argnames is ['Header', 'KeyId']
	]).

	:- public(allowed_algorithm/2).
	:- mode(allowed_algorithm(+atom, +list(compound)), one_or_error).
	:- info(allowed_algorithm/2, [
		comment is 'Validates that an algorithm is supported and allowed by the options.',
		argnames is ['Algorithm', 'Options'],
		exceptions is [
			'``Algorithm`` is unsupported or disallowed by ``Options``' - domain_error(jwt_algorithm, 'Algorithm')
		]
	]).

	:- public(supported_algorithm/1).
	:- mode(supported_algorithm(+atom), zero_or_one).
	:- info(supported_algorithm/1, [
		comment is 'Succeeds when the algorithm is supported by this library.',
		argnames is ['Algorithm']
	]).

	:- public(key_type/2).
	:- mode(key_type(+atom, -atom), zero_or_one).
	:- info(key_type/2, [
		comment is 'Maps a supported algorithm to the required JWK key type.',
		argnames is ['Algorithm', 'KeyType']
	]).

	:- public(hmac_hash/2).
	:- mode(hmac_hash(+atom, -object_identifier), zero_or_one).
	:- info(hmac_hash/2, [
		comment is 'Maps a supported HMAC algorithm to its hash object.',
		argnames is ['Algorithm', 'Hash']
	]).

	:- uses(list, [
		member/2
	]).

	header_algorithm(Header, Algorithm) :-
		(	^^json_member(alg, Header, Algorithm),
			atom(Algorithm) ->
			true
		;	domain_error(jwt_header, Header)
		).

	header_key_id(Header, KeyId) :-
		^^json_member(kid, Header, KeyId),
		atom(KeyId).

	allowed_algorithm(Algorithm, Options) :-
		(	Algorithm == none ->
			(	^^option(allow_none(true), Options) ->
				true
			;	domain_error(jwt_algorithm, Algorithm)
			)
		;	supported_algorithm(Algorithm) ->
			(	^^option(allow_algorithms(Algorithms), Options),
				member(Algorithm, Algorithms) ->
				true
			;	domain_error(jwt_algorithm, Algorithm)
			)
		;	domain_error(jwt_algorithm, Algorithm)
		).

	supported_algorithm('HS256').
	supported_algorithm('RS256').
	supported_algorithm('ES256').

	key_type('HS256', 'oct').
	key_type('RS256', 'RSA').
	key_type('ES256', 'EC').

	hmac_hash('HS256', sha256).

:- end_object.
