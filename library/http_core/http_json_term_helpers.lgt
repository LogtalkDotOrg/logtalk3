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


:- category(http_json_term_helpers).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Internal shared helpers for the repository-standard JSON term representation used by the HTTP, OpenAPI, and HTTP router libraries.'
	]).

	:- protected(json_object_pairs/2).
	:- mode(json_object_pairs(+term, -list(compound)), zero_or_one).
	:- mode(json_object_pairs(-term, +list(compound)), one).
	:- info(json_object_pairs/2, [
		comment is 'Converts between supported JSON object term representations and a flat list of key-value pairs.',
		argnames is ['Object', 'Pairs']
	]).

	:- protected(pairs_to_object/2).
	:- mode(pairs_to_object(+list(compound), -term), one).
	:- info(pairs_to_object/2, [
		comment is 'Converts a list of JSON object key-value pairs into the repository-standard object representation.',
		argnames is ['Pairs', 'Object']
	]).

	:- protected(curly_pairs_to_list/2).
	:- mode(curly_pairs_to_list(+compound, -list(compound)), one).
	:- info(curly_pairs_to_list/2, [
		comment is 'Converts a nested curly-pair term into a plain list of pairs.',
		argnames is ['CurlyPairs', 'Pairs']
	]).

	:- protected(list_to_curly_pairs/2).
	:- mode(list_to_curly_pairs(+list(compound), -compound), one).
	:- info(list_to_curly_pairs/2, [
		comment is 'Converts a plain list of pairs into the nested curly-pair representation.',
		argnames is ['Pairs', 'CurlyPairs']
	]).

	:- protected(pair_key_value/3).
	:- mode(pair_key_value(+compound, -term, -term), one).
	:- info(pair_key_value/3, [
		comment is 'Normalizes supported key-value pair syntaxes into explicit key and value terms.',
		argnames is ['Pair', 'Key', 'Value']
	]).

	:- protected(normalize_json_value/2).
	:- mode(normalize_json_value(+term, -term), one).
	:- info(normalize_json_value/2, [
		comment is 'Normalizes arbitrary JSON-like values into the repository-standard JSON representation, including booleans and null.',
		argnames is ['Value', 'JsonValue']
	]).

	:- protected(normalize_json_pairs/2).
	:- mode(normalize_json_pairs(+list(compound), -list(compound)), one).
	:- info(normalize_json_pairs/2, [
		comment is 'Normalizes the values of a JSON object pair list into the repository-standard JSON representation.',
		argnames is ['Pairs', 'JsonPairs']
	]).

	json_object_pairs({}, []) :-
		!.
	json_object_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_pairs(json(Pairs), Pairs) :-
		!.
	json_object_pairs(_, _) :-
		fail.

	pairs_to_object([], {}) :-
		!.
	pairs_to_object(Pairs, {CurlyPairs}) :-
		list_to_curly_pairs(Pairs, CurlyPairs).

	curly_pairs_to_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		curly_pairs_to_list(Rest, Pairs).
	curly_pairs_to_list(Pair, [Pair]).

	list_to_curly_pairs([Pair], Pair) :-
		!.
	list_to_curly_pairs([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly_pairs(Pairs, Rest).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

	normalize_json_value(true, @true) :-
		!.
	normalize_json_value(false, @false) :-
		!.
	normalize_json_value(null, @null) :-
		!.
	normalize_json_value([], []) :-
		!.
	normalize_json_value([Value| Values], [JsonValue| JsonValues]) :-
		!,
		normalize_json_value(Value, JsonValue),
		normalize_json_value(Values, JsonValues).
	normalize_json_value(Value, JsonValue) :-
		json_object_pairs(Value, Pairs),
		!,
		normalize_json_pairs(Pairs, JsonPairs),
		pairs_to_object(JsonPairs, JsonValue).
	normalize_json_value(Value, Value).

	normalize_json_pairs([], []).
	normalize_json_pairs([Pair| Pairs], [Key-JsonValue| JsonPairs]) :-
		pair_key_value(Pair, Key, Value),
		normalize_json_value(Value, JsonValue),
		normalize_json_pairs(Pairs, JsonPairs).

:- end_category.
