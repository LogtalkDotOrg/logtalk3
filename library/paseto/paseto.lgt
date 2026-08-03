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
		date is 2026-08-03,
		comment is 'Facade for PASETO v4 JSON claims encryption, signing, authentication, validation, and key selection.'
	]).

	:- public(encrypt/4).
	:- mode(encrypt(+term, +list(byte), -atom, +list(compound)), one_or_error).
	:- info(encrypt/4, [
		comment is 'Encrypts a JSON claims object as a v4.local token.',
		argnames is ['Claims', 'Key', 'Token', 'Options']
	]).

	:- public(decrypt/4).
	:- mode(decrypt(+atom, +term, -term, +list(compound)), zero_or_one_or_error).
	:- info(decrypt/4, [
		comment is 'Authenticates and decrypts a v4.local claims token using a key or key set.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Options']
	]).

	:- public(decrypt/5).
	:- mode(decrypt(+atom, +term, -term, -term, +list(compound)), zero_or_one_or_error).
	:- info(decrypt/5, [
		comment is 'Authenticates and decrypts a v4.local claims token and returns its authenticated footer JSON object.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Footer', 'Options']
	]).

	:- public(sign/4).
	:- mode(sign(+term, +list(byte), -atom, +list(compound)), one_or_error).
	:- info(sign/4, [
		comment is 'Signs a JSON claims object as a v4.public token using an Ed25519 seed.',
		argnames is ['Claims', 'Seed', 'Token', 'Options']
	]).

	:- public(verify/4).
	:- mode(verify(+atom, +term, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify/4, [
		comment is 'Authenticates and validates a v4.public claims token using a public key or key set.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Options']
	]).

	:- public(verify/5).
	:- mode(verify(+atom, +term, -term, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify/5, [
		comment is 'Authenticates and validates a v4.public claims token and returns its authenticated footer JSON object.',
		argnames is ['Token', 'KeyOrKeySet', 'Claims', 'Footer', 'Options']
	]).

	:- public(claims/2).
	:- mode(claims(+atom, -term), one_or_error).
	:- info(claims/2, [
		comment is 'Decodes claims from a v4.public token without authenticating them. Rejects local tokens. The result must not be trusted.',
		argnames is ['Token', 'Claims']
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
		argnames is ['Claims', 'Policy', 'Options']
	]).

	:- public(validate_claim/3).
	:- mode(validate_claim(+term, +compound, +list(compound)), one_or_error).
	:- meta_predicate(validate_claim(*, *, *)).
	:- info(validate_claim/3, [
		comment is 'Validates one claim policy.',
		argnames is ['Claims', 'ClaimPolicy', 'Options']
	]).

	:- public(peek_key_id/2).
	:- mode(peek_key_id(+atom, -atom), zero_or_one_or_error).
	:- info(peek_key_id/2, [
		comment is 'Reads a kid value from the unauthenticated token footer for key selection. The result must not be trusted.',
		argnames is ['Token', 'KeyId']
	]).

	:- public(validate_key_set/1).
	:- mode(validate_key_set(+compound), one_or_error).
	:- info(validate_key_set/1, [
		comment is 'Validates a native PASETO key_set/1 term.',
		argnames is ['KeySet']
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
