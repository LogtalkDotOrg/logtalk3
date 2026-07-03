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


:- object(json_patch,
	implements(json_patch_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'JSON Patch (RFC 6902) application predicates for native Logtalk JSON terms.'
	]).

	:- uses(json_pointer, [
		parse/2
	]).

	:- uses(list, [
		append/3, length/2, nth0/3, nth0/4, proper_prefix/2, valid/1 as proper_list/1
	]).

	apply(Patch, _, _) :-
		var(Patch),
		!,
		instantiation_error.
	apply(_, OldJSON, _) :-
		var(OldJSON),
		!,
		instantiation_error.
	apply(Patch, OldJSON, NewJSON) :-
		proper_list(Patch),
		!,
		apply_operations(Patch, OldJSON, NewJSON).
	apply(Patch, _, _) :-
		type_error(list, Patch).

	apply_operations([], JSON, JSON).
	apply_operations([Operation| Operations], OldJSON, NewJSON) :-
		operation_data(Operation, Name, Path, From, Value),
		apply_operation(Name, Path, From, Value, OldJSON, IntermediateJSON),
		apply_operations(Operations, IntermediateJSON, NewJSON).

	operation_data(Operation, Name, Path, From, Value) :-
		operation_member_text(Operation, op, Name),
		operation_member_pointer(Operation, path, Path),
		operation_optional_member(Operation, from, MaybeFrom),
		operation_optional_member(Operation, value, MaybeValue),
		operation_required_arguments(Name, MaybeFrom, From, MaybeValue, Value),
		!.
	operation_data(Operation, _, _, _, _) :-
		domain_error(json_patch_operation, Operation).

	operation_required_arguments(add, _, [], value(Value), Value).
	operation_required_arguments(remove, _, [], absent, _).
	operation_required_arguments(replace, _, [], value(Value), Value).
	operation_required_arguments(move, value(FromValue), From, absent, _) :-
		pointer_source(FromValue, Source),
		parse(Source, From).
	operation_required_arguments(copy, value(FromValue), From, absent, _) :-
		pointer_source(FromValue, Source),
		parse(Source, From).
	operation_required_arguments(test, _, [], value(Value), Value).

	apply_operation(add, Path, _, Value, OldJSON, NewJSON) :-
		add_at_path(Path, OldJSON, Value, NewJSON).
	apply_operation(remove, Path, _, _, OldJSON, NewJSON) :-
		remove_at_path(Path, OldJSON, NewJSON).
	apply_operation(replace, Path, _, Value, OldJSON, NewJSON) :-
		replace_at_path(Path, OldJSON, Value, NewJSON).
	apply_operation(move, Path, From, _, OldJSON, NewJSON) :-
		\+ proper_prefix(From, Path),
		value_at_path(From, OldJSON, Value),
		remove_at_path(From, OldJSON, IntermediateJSON),
		add_at_path(Path, IntermediateJSON, Value, NewJSON).
	apply_operation(copy, Path, From, _, OldJSON, NewJSON) :-
		value_at_path(From, OldJSON, Value),
		add_at_path(Path, OldJSON, Value, NewJSON).
	apply_operation(test, Path, _, Value, OldJSON, OldJSON) :-
		value_at_path(Path, OldJSON, ExistingValue),
		json_equal(ExistingValue, Value).

	operation_member_text(Operation, Key, Text) :-
		operation_member_value(Operation, Key, Value),
		text_atom(Value, Text).

	operation_member_pointer(Operation, Key, Pointer) :-
		operation_member_value(Operation, Key, Value),
		pointer_source(Value, Source),
		parse(Source, Pointer).

	operation_member_value(Operation, Key, Value) :-
		object_entries(Operation, _, Entries),
		entry_matches(Entries, Key, Matches),
		Matches = [match(_, entry(_, _, Value))].

	operation_optional_member(Operation, Key, Optional) :-
		object_entries(Operation, _, Entries),
		entry_matches(Entries, Key, Matches),
		optional_member_from_matches(Matches, Key, Optional).

	optional_member_from_matches([], _, absent).
	optional_member_from_matches([match(_, entry(_, _, Value))], _, value(Value)).

	pointer_source(Atom, atom(Atom)) :-
		atom(Atom),
		!.
	pointer_source(chars(Chars), chars(Chars)) :-
		!.
	pointer_source(codes(Codes), codes(Codes)).

	value_at_path([], JSON, JSON).
	value_at_path([Token| Tokens], JSON, Value) :-
		child_at(JSON, Token, Child),
		value_at_path(Tokens, Child, Value).

	add_at_path([], _, Value, Value).
	add_at_path([Token], JSON, Value, NewJSON) :-
		!,
		add_child(JSON, Token, Value, NewJSON).
	add_at_path([Token| Tokens], JSON, Value, NewJSON) :-
		child_at(JSON, Token, Child),
		add_at_path(Tokens, Child, Value, NewChild),
		replace_child(JSON, Token, NewChild, NewJSON).

	remove_at_path([Token], JSON, NewJSON) :-
		!,
		remove_child(JSON, Token, NewJSON).
	remove_at_path([Token| Tokens], JSON, NewJSON) :-
		child_at(JSON, Token, Child),
		remove_at_path(Tokens, Child, NewChild),
		replace_child(JSON, Token, NewChild, NewJSON).

	replace_at_path([], _, Value, Value).
	replace_at_path([Token], JSON, Value, NewJSON) :-
		!,
		replace_child(JSON, Token, Value, NewJSON).
	replace_at_path([Token| Tokens], JSON, Value, NewJSON) :-
		child_at(JSON, Token, Child),
		replace_at_path(Tokens, Child, Value, NewChild),
		replace_child(JSON, Token, NewChild, NewJSON).

	child_at(JSON, Token, Value) :-
		object_entries(JSON, _, Entries),
		!,
		entry_matches(Entries, Token, Matches),
		Matches = [match(_, entry(_, _, Value))].
	child_at(Array, Token, Value) :-
		proper_list(Array),
		token_existing_index(Token, Index),
		nth0(Index, Array, Value).

	add_child(JSON, Token, Value, NewJSON) :-
		object_entries(JSON, Kind, Entries),
		!,
		object_add_member(Kind, Entries, Token, Value, NewJSON).
	add_child(Array, Token, Value, NewArray) :-
		proper_list(Array),
		array_add_element(Array, Token, Value, NewArray).

	remove_child(JSON, Token, NewJSON) :-
		object_entries(JSON, Kind, Entries),
		!,
		object_remove_member(Kind, Entries, Token, NewJSON).
	remove_child(Array, Token, NewArray) :-
		proper_list(Array),
		array_remove_element(Array, Token, NewArray).

	replace_child(JSON, Token, Value, NewJSON) :-
		object_entries(JSON, Kind, Entries),
		!,
		object_replace_member(Kind, Entries, Token, Value, NewJSON).
	replace_child(Array, Token, Value, NewArray) :-
		proper_list(Array),
		array_replace_element(Array, Token, Value, NewArray).

	object_entries({}, curly, []) :-
		!.
	object_entries({Pairs}, curly, Entries) :-
		!,
		pairs_term_entries(Pairs, Entries).
	object_entries(json(Pairs), list, Entries) :-
		proper_list(Pairs),
		!,
		pairs_list_entries(Pairs, Entries).

	pairs_term_entries((Pair, Pairs), [Entry| Entries]) :-
		!,
		pair_entry(Pair, Entry),
		pairs_term_entries(Pairs, Entries).
	pairs_term_entries(Pair, [Entry]) :-
		pair_entry(Pair, Entry).

	pairs_list_entries([], []).
	pairs_list_entries([Pair| Pairs], [Entry| Entries]) :-
		pair_entry(Pair, Entry),
		pairs_list_entries(Pairs, Entries).

	pair_entry(Key-Value, entry(dash, Key, Value)) :-
		!.
	pair_entry(Key=Value, entry(equal, Key, Value)) :-
		!.
	pair_entry(':'(Key, Value), entry(colon, Key, Value)).

	entry_matches(Entries, Token, Matches) :-
		text_atom(Token, TokenAtom),
		entry_matches(Entries, TokenAtom, 0, Matches, []).

	entry_matches([], _, _, Matches, Matches).
	entry_matches([Entry| Entries], TokenAtom, Index, Matches, Tail) :-
		(   entry_key_atom(Entry, EntryAtom) ->
			(   EntryAtom == TokenAtom ->
				Matches = [match(Index, Entry)| Rest]
			;   Matches = Rest
			)
		;   Matches = Rest
		),
		NextIndex is Index + 1,
		entry_matches(Entries, TokenAtom, NextIndex, Rest, Tail).

	entry_key_atom(entry(_, Key, _), KeyAtom) :-
		text_atom(Key, KeyAtom).

	object_add_member(Kind, Entries, Token, Value, NewJSON) :-
		entry_matches(Entries, Token, Matches),
		(   Matches == [] ->
			new_entry(Entries, Token, Value, Entry),
			append(Entries, [Entry], NewEntries)
		;   Matches = [match(Index, entry(Representation, Key, _))] ->
			NewEntry = entry(Representation, Key, Value),
			replace_entry_at(Index, Entries, NewEntry, NewEntries)
		;   fail
		),
		entries_object(Kind, NewEntries, NewJSON).

	object_remove_member(Kind, Entries, Token, NewJSON) :-
		entry_matches(Entries, Token, [match(Index, _)]),
		remove_entry_at(Index, Entries, NewEntries),
		entries_object(Kind, NewEntries, NewJSON).

	object_replace_member(Kind, Entries, Token, Value, NewJSON) :-
		entry_matches(Entries, Token, [match(Index, entry(Representation, Key, _))]),
		NewEntry = entry(Representation, Key, Value),
		replace_entry_at(Index, Entries, NewEntry, NewEntries),
		entries_object(Kind, NewEntries, NewJSON).

	new_entry([entry(Representation, TemplateKey, _)| _], Token, Value, entry(Representation, Key, Value)) :-
		!,
		key_like(TemplateKey, Token, Key).
	new_entry([], Token, Value, entry(dash, Key, Value)) :-
		key_like('', Token, Key).

	key_like(chars(_), Token, chars(Chars)) :-
		!,
		text_atom(Token, Atom),
		atom_chars(Atom, Chars).
	key_like(codes(_), Token, codes(Codes)) :-
		!,
		text_atom(Token, Atom),
		atom_codes(Atom, Codes).
	key_like(_, Token, Atom) :-
		text_atom(Token, Atom).

	entries_object(curly, [], {}) :-
		!.
	entries_object(curly, Entries, {Pairs}) :-
		!,
		entries_pairs_term(Entries, Pairs).
	entries_object(list, Entries, json(Pairs)) :-
		entries_pairs_list(Entries, Pairs).

	entries_pairs_term([Entry], Pair) :-
		!,
		entry_pair(Entry, Pair).
	entries_pairs_term([Entry| Entries], (Pair, Pairs)) :-
		entry_pair(Entry, Pair),
		entries_pairs_term(Entries, Pairs).

	entries_pairs_list([], []).
	entries_pairs_list([Entry| Entries], [Pair| Pairs]) :-
		entry_pair(Entry, Pair),
		entries_pairs_list(Entries, Pairs).

	entry_pair(entry(dash, Key, Value), Key-Value) :-
		!.
	entry_pair(entry(equal, Key, Value), Key=Value) :-
		!.
	entry_pair(entry(colon, Key, Value), ':'(Key, Value)).

	replace_entry_at(0, [_| Entries], Entry, [Entry| Entries]) :-
		!.
	replace_entry_at(Index, [Head| Entries], Entry, [Head| NewEntries]) :-
		Index > 0,
		NextIndex is Index - 1,
		replace_entry_at(NextIndex, Entries, Entry, NewEntries).

	remove_entry_at(0, [_| Entries], Entries) :-
		!.
	remove_entry_at(Index, [Head| Entries], [Head| NewEntries]) :-
		Index > 0,
		NextIndex is Index - 1,
		remove_entry_at(NextIndex, Entries, NewEntries).

	array_add_element(Array, Token, Value, NewArray) :-
		length(Array, Length),
		token_add_index(Token, Length, Index),
		nth0(Index, NewArray, Value, Array).

	array_remove_element(Array, Token, NewArray) :-
		token_existing_index(Token, Index),
		nth0(Index, Array, _, NewArray).

	array_replace_element(Array, Token, Value, NewArray) :-
		token_existing_index(Token, Index),
		nth0(Index, Array, _, Rest),
		nth0(Index, NewArray, Value, Rest).

	token_add_index(Token, Length, Length) :-
		token_is_dash(Token),
		!.
	token_add_index(Token, Length, Index) :-
		token_existing_index(Token, Index),
		Index =< Length.

	token_existing_index(Token, Index) :-
		text_codes(Token, Codes),
		array_index_codes(Codes, Index).

	token_is_dash(Token) :-
		text_codes(Token, [0'-]).

	array_index_codes([0'0], 0) :-
		!.
	array_index_codes([First| Rest], Index) :-
		First >= 0'1,
		First =< 0'9,
		decimal_digit_codes(Rest),
		number_codes(Index, [First| Rest]).

	decimal_digit_codes([]).
	decimal_digit_codes([Code| Codes]) :-
		Code >= 0'0,
		Code =< 0'9,
		decimal_digit_codes(Codes).

	json_equal(Left, Right) :-
		normalized_object_entries(Left, LeftEntries),
		normalized_object_entries(Right, RightEntries),
		!,
		length(LeftEntries, LeftLength),
		length(RightEntries, RightLength),
		LeftLength =:= RightLength,
		matching_normalized_entries(LeftEntries, RightEntries).
	json_equal(Left, Right) :-
		proper_list(Left),
		proper_list(Right),
		!,
		json_lists_equal(Left, Right).
	json_equal(Left, Right) :-
		number(Left),
		number(Right),
		!,
		Left =:= Right.
	json_equal(Left, Right) :-
		text_atom(Left, Text),
		text_atom(Right, Text),
		!.
	json_equal(Left, Right) :-
		Left == Right.

	normalized_object_entries(Object, NormalizedEntries) :-
		object_entries(Object, _, Entries),
		normalized_object_entries(Entries, [], NormalizedEntries).

	normalized_object_entries([], _, []).
	normalized_object_entries([entry(_, Key, Value)| Entries], Seen, [nentry(KeyAtom, Value)| NormalizedEntries]) :-
		text_atom(Key, KeyAtom),
		\+ seen_key(Seen, KeyAtom),
		normalized_object_entries(Entries, [KeyAtom| Seen], NormalizedEntries).

	seen_key([Seen| _], Key) :-
		Seen == Key,
		!.
	seen_key([_| Seen], Key) :-
		seen_key(Seen, Key).

	matching_normalized_entries([], _).
	matching_normalized_entries([nentry(Key, Value)| Entries], OtherEntries) :-
		normalized_entry_value(OtherEntries, Key, OtherValue),
		json_equal(Value, OtherValue),
		matching_normalized_entries(Entries, OtherEntries).

	normalized_entry_value([nentry(Key, Value)| _], WantedKey, Value) :-
		Key == WantedKey,
		!.
	normalized_entry_value([_| Entries], WantedKey, Value) :-
		normalized_entry_value(Entries, WantedKey, Value).

	json_lists_equal([], []).
	json_lists_equal([Left| Lefts], [Right| Rights]) :-
		json_equal(Left, Right),
		json_lists_equal(Lefts, Rights).

	text_atom(Atom, Atom) :-
		atom(Atom),
		!.
	text_atom(chars(Chars), Atom) :-
		!,
		atom_chars(Atom, Chars).
	text_atom(codes(Codes), Atom) :-
		!,
		atom_codes(Atom, Codes).

	text_codes(Text, Codes) :-
		text_atom(Text, Atom),
		atom_codes(Atom, Codes).

:- end_object.
