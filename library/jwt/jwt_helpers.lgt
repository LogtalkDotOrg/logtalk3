%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(jwt_helpers,
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-06,
		comment is 'Internal shared helpers for the jwt library.'
	]).

	:- protected(json_member/3).
	:- mode(json_member(+atom, +term, -term), zero_or_more).
	:- info(json_member/3, [
		comment is 'Enumerates JSON object member values for a given key.',
		argnames is ['Key', 'Object', 'Value']
	]).

	:- protected(json_member_default/4).
	:- mode(json_member_default(+atom, +term, +term, -term), one).
	:- info(json_member_default/4, [
		comment is 'Returns a JSON object member value or a default when the key is absent.',
		argnames is ['Key', 'Object', 'Default', 'Value']
	]).

	:- protected(json_object_pairs/2).
	:- mode(json_object_pairs(+term, -list(compound)), zero_or_one).
	:- info(json_object_pairs/2, [
		comment is 'Extracts the key-value pairs from a JSON object term.',
		argnames is ['Object', 'Pairs']
	]).

	:- protected(json_object/1).
	:- mode(json_object(+term), one_or_error).
	:- info(json_object/1, [
		comment is 'Validates that a JSON term is an object without duplicate member names.',
		argnames is ['Object'],
		exceptions is [
			'``Object`` is not a JSON object or contains duplicate member names' - domain_error(jwt_json_object, 'Object')
		]
	]).

	:- protected(pair_key_value/3).
	:- mode(pair_key_value(+compound, -term, -term), one).
	:- info(pair_key_value/3, [
		comment is 'Extracts the key and value from a JSON pair compound term.',
		argnames is ['Pair', 'Key', 'Value']
	]).

	:- protected(base64url_atom_bytes/2).
	:- mode(base64url_atom_bytes(+atom, -list(byte)), one_or_error).
	:- mode(base64url_atom_bytes(-atom, +list(byte)), one_or_error).
	:- info(base64url_atom_bytes/2, [
		comment is 'Converts between unpadded Base64URL atoms and byte lists.',
		argnames is ['Atom', 'Bytes'],
		exceptions is [
			'``Atom`` is a variable or the sink is a variable' - instantiation_error,
			'``Atom`` is neither a variable nor a valid Base64URL source term' - domain_error(base64url_source, codes('Codes')),
			'``Bytes`` cannot be encoded in the requested Base64URL sink representation' - domain_error(base64url_sink, codes('Codes')),
			'``Atom`` contains Base64URL data with characters outside the Base64URL alphabet' - representation_error(base64)
		]
	]).

	:- protected(atom_bytes/2).
	:- mode(atom_bytes(+atom, -list(byte)), one).
	:- mode(atom_bytes(-atom, +list(byte)), one).
	:- info(atom_bytes/2, [
		comment is 'Converts between an atom and its byte list representation.',
		argnames is ['Atom', 'Bytes']
	]).

	:- protected(bytes_atom/2).
	:- mode(bytes_atom(+list(byte), -atom), one).
	:- info(bytes_atom/2, [
		comment is 'Converts a byte list into an atom.',
		argnames is ['Bytes', 'Atom']
	]).

	:- protected(atom_list/1).
	:- mode(atom_list(+list(atom)), zero_or_one).
	:- info(atom_list/1, [
		comment is 'Validates that a list contains only atoms.',
		argnames is ['Atoms']
	]).

	:- uses(list, [
		member/2
	]).

	valid_option(allow_algorithms(Algorithms)) :-
		atom_list(Algorithms).
	valid_option(allow_none(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(allow_missing_exp(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(clock_skew(ClockSkew)) :-
		number(ClockSkew),
		ClockSkew >= 0.
	valid_option(now(Now)) :-
		number(Now).
	valid_option(max_age(MaxAge)) :-
		number(MaxAge),
		MaxAge >= 0.
	valid_option(required_claims(RequiredClaims)) :-
		atom_list(RequiredClaims).
	valid_option(claim_policy(Policy)) :-
		list::valid(Policy).
	valid_option(openssl_executable(Executable)) :-
		atom(Executable).
	valid_option(openssl_arguments(Arguments)) :-
		list::valid(Arguments).
	valid_option(algorithm(Algorithm)) :-
		atom(Algorithm).

	default_option(allow_algorithms(['HS256', 'RS256', 'ES256'])).
	default_option(allow_none(false)).
	default_option(allow_missing_exp(false)).
	default_option(clock_skew(60)).
	default_option(required_claims([])).
	default_option(claim_policy([])).

	json_member(Key, Object, Value) :-
		json_object_pairs(Object, Pairs),
		json_pair_member(Pairs, Key, Value).

	json_object(Object) :-
		( 	json_object_pairs(Object, Pairs),
			list::valid(Pairs) ->
			unique_pair_keys(Pairs, [], Object)
		; 	domain_error(jwt_json_object, Object)
		).

	json_member_default(Key, Object, Default, Value) :-
		(	json_member(Key, Object, Value0) ->
			Value = Value0
		;	Value = Default
		).

	json_pair_member([Pair| _], Key, Value) :-
		pair_key_value(Pair, Key, Value),
		!.
	json_pair_member([_| Pairs], Key, Value) :-
		json_pair_member(Pairs, Key, Value).

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

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

	unique_pair_keys([], _, _).
	unique_pair_keys([Pair| Pairs], Seen, Object) :-
		( 	pair_key_value(Pair, Key, _) ->
			( 	member(Key, Seen) ->
				domain_error(jwt_json_object, Object)
			; 	unique_pair_keys(Pairs, [Key| Seen], Object)
			)
		; 	domain_error(jwt_json_object, Object)
		).

	base64url_atom_bytes(Atom, Bytes) :-
		atom(Atom),
		!,
		atom_codes(Atom, Codes),
		base64url_no_padding::parse(codes(Codes), Bytes).
	base64url_atom_bytes(Atom, Bytes) :-
		base64url_no_padding::generate(codes(Codes), Bytes),
		atom_codes(Atom, Codes).

	atom_bytes(Atom, Bytes) :-
		atom_codes(Atom, Bytes).

	bytes_atom(Bytes, Atom) :-
		atom_codes(Atom, Bytes).

	atom_list([]).
	atom_list([Atom| Atoms]) :-
		atom(Atom),
		atom_list(Atoms).

:- end_category.
