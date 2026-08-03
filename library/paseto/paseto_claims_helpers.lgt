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


:- category(paseto_claims_helpers,
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Shared JSON and option helpers for PASETO claims workflows.'
	]).

	:- protected(json_member/3).
	:- mode(json_member(+atom, +term, -term), zero_or_more).
	:- info(json_member/3, [
		comment is 'Enumerates JSON object member values for a key.',
		argnames is ['Key', 'Object', 'Value']
	]).

	:- protected(json_object_pairs/2).
	:- mode(json_object_pairs(+term, -list(compound)), zero_or_one).
	:- info(json_object_pairs/2, [
		comment is 'Extracts pairs from a JSON object term.',
		argnames is ['Object', 'Pairs']
	]).

	:- protected(json_object/1).
	:- mode(json_object(+term), one_or_error).
	:- info(json_object/1, [
		comment is 'Validates a JSON object and rejects duplicate member names.',
		argnames is ['Object'],
		exceptions is [
			'``Object`` is not a JSON object or contains duplicate members' - domain_error(paseto_json_object, 'Object')
		]
	]).

	:- protected(json_bytes/2).
	:- mode(json_bytes(+term, -list(byte)), one_or_error).
	:- mode(json_bytes(-term, +list(byte)), one_or_error).
	:- info(json_bytes/2, [
		comment is 'Converts between a JSON term and its UTF-8-compatible byte representation.',
		argnames is ['JSON', 'Bytes']
	]).

	:- protected(byte_list/1).
	:- mode(byte_list(+term), zero_or_one).
	:- info(byte_list/1, [
		comment is 'Succeeds when the argument is a proper list of bytes.',
		argnames is ['Bytes']
	]).

	:- uses(list, [
		member/2
	]).

	valid_option(allow_missing_exp(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(clock_skew(ClockSkew)) :-
		number(ClockSkew), ClockSkew >= 0.
	valid_option(now(Now)) :-
		number(Now).
	valid_option(max_age(MaxAge)) :-
		number(MaxAge), MaxAge >= 0.
	valid_option(required_claims(RequiredClaims)) :-
		atom_list(RequiredClaims).
	valid_option(claim_policy(Policy)) :-
		list::valid(Policy).
	valid_option(footer(Footer)) :-
		nonvar(Footer).
	valid_option(key_id(KeyId)) :-
		atom(KeyId).
	valid_option(implicit_assertion(Bytes)) :-
		byte_list(Bytes).

	default_option(allow_missing_exp(false)).
	default_option(clock_skew(60)).
	default_option(required_claims([])).
	default_option(claim_policy([])).
	default_option(implicit_assertion([])).

	json_member(Key, Object, Value) :-
		json_object_pairs(Object, Pairs),
		json_pair_member(Pairs, Key, Value).

	json_object(Object) :-
		(	json_object_pairs(Object, Pairs), list::valid(Pairs) ->
			unique_pair_keys(Pairs, [], Object)
		;	domain_error(paseto_json_object, Object)
		).

	json_object_pairs({}, []) :-
		!.
	json_object_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_pairs(json(Pairs), Pairs) :-
		!.

	curly_pairs_to_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		curly_pairs_to_list(Rest, Pairs).
	curly_pairs_to_list(Pair, [Pair]).

	json_pair_member([Pair| _], Key, Value) :-
		pair_key_value(Pair, Key, Value),
		!.
	json_pair_member([_| Pairs], Key, Value) :-
		json_pair_member(Pairs, Key, Value).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

	unique_pair_keys([], _, _) :-
		!.
	unique_pair_keys([Pair| Pairs], Seen, Object) :-
		(	pair_key_value(Pair, Key, _) ->
			(	member(Key, Seen) ->
				domain_error(paseto_json_object, Object)
			;	unique_pair_keys(Pairs, [Key| Seen], Object)
			)
		;	domain_error(paseto_json_object, Object)
		).

	json_bytes(JSON, Bytes) :-
		nonvar(JSON),
		!,
		json::generate(codes(Bytes), JSON).
	json_bytes(JSON, Bytes) :-
		json::parse(codes(Bytes), JSON).

	byte_list(Bytes) :-
		list::valid(Bytes),
		byte_list_elements(Bytes).

	byte_list_elements([]) :-
		!.
	byte_list_elements([Byte| Bytes]) :-
		integer(Byte), Byte >= 0, Byte =< 255,
		byte_list_elements(Bytes).

	atom_list([]) :-
		!.
	atom_list([Atom| Atoms]) :-
		atom(Atom),
		atom_list(Atoms).

:- end_category.
