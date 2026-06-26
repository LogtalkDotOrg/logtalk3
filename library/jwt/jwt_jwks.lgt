%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_jwks,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'JSON Web Key Set validation and key selection helpers.'
	]).

	:- public(validate/1).
	:- mode(validate(+term), one_or_error).
	:- info(validate/1, [
		comment is 'Validates that a term is a JSON Web Key Set.',
		argnames is ['JWKSet'],
		exceptions is [
			'``JWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(jwt_jwks, 'JWKSet')
		]
	]).

	:- public(select_key/3).
	:- mode(select_key(+term, +term, -term), one_or_error).
	:- info(select_key/3, [
		comment is 'Selects a matching verification key from a JWK Set for a JWT header.',
		argnames is ['JWKSet', 'Header', 'Key'],
		exceptions is [
			'``JWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(jwt_jwks, 'JWKSet'),
			'``Header`` does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'No compatible key exists for ``Header``' - existence_error(jwt_jwk, 'Header')
		]
	]).

	:- public(select_key/4).
	:- mode(select_key(+term, +term, -term, +list(compound)), one_or_error).
	:- info(select_key/4, [
		comment is 'Selects a matching verification key from a JWK Set for a JWT header and options.',
		argnames is ['JWKSet', 'Header', 'Key', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``JWKSet`` is not a JSON Web Key Set with a ``keys`` list' - domain_error(jwt_jwks, 'JWKSet'),
			'``Header`` does not contain a valid ``alg`` member' - domain_error(jwt_header, 'Header'),
			'The JWT algorithm is unsupported or disallowed' - domain_error(jwt_algorithm, 'Algorithm'),
			'No compatible key exists for ``Header``' - existence_error(jwt_jwk, 'Header')
		]
	]).

	:- uses(list, [
		member/2
	]).

	validate(JWKSet) :-
		jwks_keys(JWKSet, _Keys).

	select_key(JWKSet, Header, Key) :-
		select_key(JWKSet, Header, Key, []).

	select_key(JWKSet, Header, Key, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		jwks_keys(JWKSet, Keys),
		jwt_jwa::header_algorithm(Header, Algorithm),
		jwt_jwa::allowed_algorithm(Algorithm, MergedOptions),
		(	jwt_jwa::header_key_id(Header, KeyId) ->
			true
		;	KeyId = none
		),
		select_matching_key(Keys, Algorithm, KeyId, Key),
		!.
	select_key(_JWKSet, Header, _Key, _Options) :-
		existence_error(jwt_jwk, Header).

	jwks_keys(JWKSet, Keys) :-
		(	^^json_member(keys, JWKSet, Keys),
			list::valid(Keys) ->
			true
		;	domain_error(jwt_jwks, JWKSet)
		).

	select_matching_key([Key| _], Algorithm, KeyId, Key) :-
		matching_key(Key, Algorithm, KeyId),
		!.
	select_matching_key([_| Keys], Algorithm, KeyId, Key) :-
		select_matching_key(Keys, Algorithm, KeyId, Key).

	matching_key(Key, Algorithm, KeyId) :-
		jwt_jwk::compatible_key(Key, Algorithm),
		matching_curve(Key, Algorithm),
		matching_key_id(Key, KeyId),
		matching_key_algorithm(Key, Algorithm),
		matching_key_use(Key).

	matching_curve(Key, 'ES256') :-
		!,
		^^json_member(crv, Key, 'P-256').
	matching_curve(_Key, _Algorithm).

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
