%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(paseto,
	imports([paseto_helpers, paseto_claims_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Facade for PASETO v4 JSON claims encryption, signing, authentication, validation, and key selection.'
	]).

	:- public(encrypt/4).
	:- mode(encrypt(+term, +list(byte), -atom, +list(compound)), one_or_error).
	:- info(encrypt/4, [
		comment is 'Encrypts a JSON claims object as a v4.local token.',
		argnames is ['Claims', 'Key', 'Token', 'Options'],
		exceptions is [
			'``Claims`` is not a JSON object or contains duplicate members' - domain_error(paseto_json_object, 'Claims'),
			'``Key`` is not a list of 32 bytes' - type_error(list(byte, 32), 'Key'),
			'The footer ``kid`` conflicts with the ``key_id/1`` option' - domain_error(paseto_footer_key_id, 'Existing'-'KeyId'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(decrypt/4).
	:- mode(decrypt(+atom, +term, -term, +list(compound)), zero_or_one_or_error).
	:- info(decrypt/4, [
		comment is 'Authenticates and decrypts a v4.local claims token using a key or key set.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Options'],
		exceptions is [
			'``Token`` is not a canonical v4.local token' - domain_error(paseto_v4_token, 'Token'),
			'``KeyOrKeySet`` is an invalid key set' - domain_error(paseto_key_set, 'KeyOrKeySet'),
			'No key can be selected from ``KeyOrKeySet``' - existence_error(paseto_key, local-'KeyId'),
			'The authenticated payload is not a JSON object' - domain_error(paseto_json_object, 'Claims'),
			'The claims do not satisfy the validation policy' - domain_error(paseto_claims, 'Reason'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(decrypt/5).
	:- mode(decrypt(+atom, +term, -term, -term, +list(compound)), zero_or_one_or_error).
	:- info(decrypt/5, [
		comment is 'Authenticates and decrypts a v4.local claims token and returns its authenticated footer JSON object.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Footer', 'Options'],
		exceptions is [
			'``Token`` is not a canonical v4.local token' - domain_error(paseto_v4_token, 'Token'),
			'``KeyOrKeySet`` is an invalid key set' - domain_error(paseto_key_set, 'KeyOrKeySet'),
			'No key can be selected from ``KeyOrKeySet``' - existence_error(paseto_key, local-'KeyId'),
			'The authenticated payload or footer is not a JSON object' - domain_error(paseto_json_object, 'JSON'),
			'The claims do not satisfy the validation policy' - domain_error(paseto_claims, 'Reason'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(sign/4).
	:- mode(sign(+term, +list(byte), -atom, +list(compound)), one_or_error).
	:- info(sign/4, [
		comment is 'Signs a JSON claims object as a v4.public token using an Ed25519 seed.',
		argnames is ['Claims', 'Seed', 'Token', 'Options'],
		exceptions is [
			'``Claims`` is not a JSON object or contains duplicate members' - domain_error(paseto_json_object, 'Claims'),
			'``Seed`` is not a list of 32 bytes' - type_error(list(byte, 32), 'Seed'),
			'The footer ``kid`` conflicts with the ``key_id/1`` option' - domain_error(paseto_footer_key_id, 'Existing'-'KeyId'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(verify/4).
	:- mode(verify(+atom, +term, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify/4, [
		comment is 'Authenticates and validates a v4.public claims token using a public key or key set.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Options'],
		exceptions is [
			'``Token`` is not a canonical v4.public token' - domain_error(paseto_v4_token, 'Token'),
			'``KeyOrKeySet`` is an invalid key set' - domain_error(paseto_key_set, 'KeyOrKeySet'),
			'No key can be selected from ``KeyOrKeySet``' - existence_error(paseto_key, public-'KeyId'),
			'The authenticated payload is not a JSON object' - domain_error(paseto_json_object, 'Claims'),
			'The claims do not satisfy the validation policy' - domain_error(paseto_claims, 'Reason'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(verify/5).
	:- mode(verify(+atom, +term, -term, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify/5, [
		comment is 'Authenticates and validates a v4.public claims token and returns its authenticated footer JSON object.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Footer', 'Options'],
		exceptions is [
			'``Token`` is not a canonical v4.public token' - domain_error(paseto_v4_token, 'Token'),
			'``KeyOrKeySet`` is an invalid key set' - domain_error(paseto_key_set, 'KeyOrKeySet'),
			'No key can be selected from ``KeyOrKeySet``' - existence_error(paseto_key, public-'KeyId'),
			'The authenticated payload or footer is not a JSON object' - domain_error(paseto_json_object, 'JSON'),
			'The claims do not satisfy the validation policy' - domain_error(paseto_claims, 'Reason'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(claims/2).
	:- mode(claims(+atom, -term), one_or_error).
	:- info(claims/2, [
		comment is 'Decodes claims from a v4.public token without authenticating them. Rejects local tokens. The result must not be trusted.',
		argnames is ['Token', 'Claims'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is not a canonical v4.public token' - domain_error(paseto_v4_token, 'Token'),
			'``Token`` has a malformed public payload' - domain_error(paseto_v4_public_payload, 'Token'),
			'``Token`` payload is not a JSON object' - domain_error(paseto_json_object, 'Claims')
		]
	]).

	:- public(claim/3).
	:- mode(claim(+term, +atom, -term), zero_or_one).
	:- info(claim/3, [
		comment is 'Looks up a value in a claims object.',
		argnames is ['Claims', 'Name', 'Value']
	]).

	:- public(validate_claims/3).
	:- mode(validate_claims(+term, +list(compound), +list(compound)), one_or_error).
	:- info(validate_claims/3, [
		comment is 'Validates a claims object using a policy list and options.',
		argnames is ['Claims', 'Policy', 'Options'],
		exceptions is [
			'``Claims`` is not a JSON object or contains duplicate members' - domain_error(paseto_json_object, 'Claims'),
			'``Claims`` is missing a required claim ``Name``' - domain_error(paseto_claims, missing('Name')),
			'``Policy`` contains an invalid claim policy' - domain_error(paseto_claim_policy, 'ClaimPolicy'),
			'A time claim has a non-numeric value' - type_error(time_number, 'Name'-'Value'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(validate_claim/3).
	:- mode(validate_claim(+term, +compound, +list(compound)), one_or_error).
	:- meta_predicate(validate_claim(*, *, *)).
	:- info(validate_claim/3, [
		comment is 'Validates one claim policy.',
		argnames is ['Claims', 'ClaimPolicy', 'Options'],
		exceptions is [
			'``Claims`` is missing the required claim ``Name``' - domain_error(paseto_claims, missing('Name')),
			'The value of claim ``Name`` does not satisfy the policy' - domain_error(paseto_claim('Name'), 'Value'),
			'``ClaimPolicy`` is not a supported claim policy' - domain_error(paseto_claim_policy, 'ClaimPolicy'),
			'A time claim has an unknown validation kind' - domain_error(paseto_time_claim_kind, 'Kind'),
			'A time claim has a non-numeric value' - type_error(time_number, 'Name'-'Value'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(peek_key_id/2).
	:- mode(peek_key_id(+atom, -atom), zero_or_one_or_error).
	:- info(peek_key_id/2, [
		comment is 'Reads a kid value from the unauthenticated token footer for key selection. The result must not be trusted.',
		argnames is ['Token', 'KeyId'],
		exceptions is [
			'``Token`` is a variable' - instantiation_error,
			'``Token`` is neither a variable nor an atom' - type_error(atom, 'Token'),
			'``Token`` is not a canonical v4 token' - domain_error(paseto_v4_token, 'Token'),
			'``Token`` footer is not a JSON object' - domain_error(paseto_json_object, 'Footer')
		]
	]).

	:- public(validate_key_set/1).
	:- mode(validate_key_set(+compound), one_or_error).
	:- info(validate_key_set/1, [
		comment is 'Validates a native PASETO key_set/1 term.',
		argnames is ['KeySet'],
		exceptions is [
			'``KeySet`` is not a valid ``key_set/1`` term' - domain_error(paseto_key_set, 'KeySet'),
			'``KeySet`` contains an invalid key record ``Record``' - domain_error(paseto_key_record, 'Record')
		]
	]).

	:- uses(list, [
		append/3, length/2
	]).

	encrypt(Claims, Key, Token, Options) :-
		prepare(Claims, Options, Payload, FooterBytes, ImplicitAssertion, _),
		paseto_v4::local_encrypt(Key, Payload, FooterBytes, ImplicitAssertion, Token),
		!.

	decrypt(Token, KeyOrKeySet, Claims, Options) :-
		decrypt(Token, KeyOrKeySet, Claims, _Footer, Options).

	decrypt(Token, KeyOrKeySet, Claims, Footer, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(implicit_assertion(ImplicitAssertion), MergedOptions),
		verification_keys(Token, local, KeyOrKeySet, Keys),
		decrypt_with_keys(Keys, Token, ImplicitAssertion, Payload, FooterBytes),
		parse_authenticated_json(Payload, Claims),
		parse_footer(FooterBytes, Footer),
		^^option(claim_policy(Policy), MergedOptions),
		paseto_claims::validate_claims(Claims, Policy, MergedOptions),
		!.

	sign(Claims, Seed, Token, Options) :-
		prepare(Claims, Options, Payload, FooterBytes, ImplicitAssertion, _),
		paseto_v4::public_sign(Seed, Payload, FooterBytes, ImplicitAssertion, Token),
		!.

	verify(Token, KeyOrKeySet, Claims, Options) :-
		verify(Token, KeyOrKeySet, Claims, _Footer, Options).

	verify(Token, KeyOrKeySet, Claims, Footer, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(implicit_assertion(ImplicitAssertion), MergedOptions),
		verification_keys(Token, public, KeyOrKeySet, Keys),
		verify_with_keys(Keys, Token, ImplicitAssertion, Payload, FooterBytes),
		parse_authenticated_json(Payload, Claims),
		parse_footer(FooterBytes, Footer),
		^^option(claim_policy(Policy), MergedOptions),
		paseto_claims::validate_claims(Claims, Policy, MergedOptions),
		!.

	claims(Token, Claims) :-
		^^parse_token(Token, public, Body, _),
		length(Body, Length),
		(	Length >= 64 ->
			true
		;	domain_error(paseto_v4_public_payload, Token)
		),
		PayloadLength is Length - 64,
		length(Payload, PayloadLength),
		append(Payload, _, Body),
		parse_authenticated_json(Payload, Claims),
		!.

	claim(Claims, Name, Value) :-
		paseto_claims::claim(Claims, Name, Value).

	validate_claims(Claims, Policy, Options) :-
		paseto_claims::validate_claims(Claims, Policy, Options).

	validate_claim(Claims, ClaimPolicy, Options) :-
		paseto_claims::validate_claim(Claims, ClaimPolicy, Options).

	peek_key_id(Token, KeyId) :-
		paseto_v4::footer(Token, FooterBytes),
		FooterBytes \== [],
		^^json_bytes(Footer, FooterBytes),
		^^json_object(Footer),
		^^json_member(kid, Footer, KeyId),
		atom(KeyId).

	validate_key_set(KeySet) :-
		paseto_keys::validate(KeySet).

	prepare(Claims, Options, Payload, FooterBytes, ImplicitAssertion, Footer) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^json_object(Claims),
		^^json_bytes(Claims, Payload),
		prepare_footer(MergedOptions, Footer, FooterBytes),
		^^option(implicit_assertion(ImplicitAssertion), MergedOptions).

	prepare_footer(Options, Footer, FooterBytes) :-
		(	^^option(footer(Footer0), Options) ->
			^^json_object(Footer0)
		;	Footer0 = {}
		),
		(	^^option(key_id(KeyId), Options) ->
			merge_key_id(Footer0, KeyId, Footer)
		;	Footer = Footer0
		),
		(	Footer == {} ->
			FooterBytes = []
		;	^^json_bytes(Footer, FooterBytes)
		).

	merge_key_id(Footer, KeyId, Merged) :-
		(	^^json_member(kid, Footer, Existing) ->
			(	Existing == KeyId ->
				Merged = Footer
			;	domain_error(paseto_footer_key_id, Existing-KeyId)
			)
		;	^^json_object_pairs(Footer, Pairs),
			json_from_pairs([kid-KeyId| Pairs], Merged)
		).

	json_from_pairs([], {}) :-
		!.
	json_from_pairs(Pairs, {Conjunction}) :-
		pairs_conjunction(Pairs, Conjunction).

	pairs_conjunction([Pair], Pair) :-
		!.
	pairs_conjunction([Pair| Pairs], (Pair, Conjunction)) :-
		pairs_conjunction(Pairs, Conjunction).

	verification_keys(Token, Purpose, KeySet, Keys) :-
		KeySet = key_set(_),
		!,
		(	catch(peek_key_id(Token, KeyId), _, fail) ->
			true
		;	KeyId = none
		),
		paseto_keys::select_keys(KeySet, Purpose, KeyId, Keys).
	verification_keys(_, _, Key, [Key]).

	decrypt_with_keys([Key| Keys], Token, ImplicitAssertion, Payload, Footer) :-
		(	paseto_v4::local_decrypt(Token, Key, ImplicitAssertion, Payload0, Footer0) ->
			Payload = Payload0, Footer = Footer0
		;	decrypt_with_keys(Keys, Token, ImplicitAssertion, Payload, Footer)
		).

	verify_with_keys([Key| Keys], Token, ImplicitAssertion, Payload, Footer) :-
		(	paseto_v4::public_verify(Token, Key, ImplicitAssertion, Payload0, Footer0) ->
			Payload = Payload0, Footer = Footer0
		;	verify_with_keys(Keys, Token, ImplicitAssertion, Payload, Footer)
		).

	parse_authenticated_json(Bytes, JSON) :-
		^^json_bytes(JSON, Bytes),
		^^json_object(JSON).

	parse_footer([], {}).
	parse_footer(Bytes, Footer) :-
		Bytes \== [],
		parse_authenticated_json(Bytes, Footer).

:- end_object.
