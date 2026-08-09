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


:- object(sqids(_Representation_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-09,
		comment is 'Encoding and decoding of short, obfuscated, URL-safe ids from lists of non-negative integers, per the Sqids specification.',
		remarks is [
			'Specification' - 'https://github.com/sqids/sqids-spec',
			'Homepage' - 'https://sqids.org',
			'Default blocklist' - 'https://github.com/sqids/sqids-blocklist (not bundled; see blocklist/1 option).'
		],
		parameters is [
			'Representation' - 'Text representation for identifiers. Possible values are ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [sqids, ids(_,_), cuid2(_,_,_), ksuid(_,_), nanoid(_,_,_), snowflakeid(_,_,_,_,_,_,_), typeid(_), ulid(_), uuid(_)]
	]).

	:- public(encode/2).
	:- mode(encode(+list(integer), -text), one).
	:- info(encode/2, [
		comment is 'Encodes a list of non-negative integers into an identifier using the default options.',
		argnames is ['Numbers', 'Id'],
		exceptions is [
			'``Numbers`` is a variable or a partial list' - instantiation_error,
			'``Numbers`` is neither a variable nor a list' - type_error(list(non_negative_integer), 'Numbers'),
			'An element ``Number`` of the list ``Numbers`` is not an integer' - type_error(integer, 'Number'),
			'An element ``Number`` of the list ``Numbers`` of Numbers is a negative integer' - domain_error(non_negative_integer, 'Number'),
			'Too many blocklist collisions occurred while generating the id' - resource_error(blocklist_retries)
		]
	]).

	:- public(encode/3).
	:- mode(encode(+list(integer), -text, +list(compound)), one_or_error).
	:- info(encode/3, [
		comment is 'Encodes a list of non-negative integers into an id using the given options. Recognized options are alphabet(Atom) (an atom with at least three unique characters; default is the standard 62-character alphabet), min_length(Integer) (an integer between 0 and 255; default is 0), and blocklist(list(atom)) (words that must not occur in the generated id; default is []). If Numbers is empty, Id is unified with the empty atom regardless of the other options.',
		argnames is ['Numbers', 'Id', 'Options'],
		exceptions is [
			'``Numbers`` is a variable or a partial list' - instantiation_error,
			'``Numbers`` is neither a variable nor a list' - type_error(list(non_negative_integer), 'Numbers'),
			'An element ``Number`` of the list ``Numbers`` is not an integer' - type_error(integer, 'Number'),
			'An element ``Number`` of the list ``Numbers`` of Numbers is a negative integer' - domain_error(non_negative_integer, 'Number'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'Too many blocklist collisions occurred while generating the id' - resource_error(blocklist_retries)
		]
	]).

	:- public(decode/2).
	:- mode(decode(+text, -list(integer)), one_or_error).
	:- info(decode/2, [
		comment is 'Decodes an identifier back into a list of non-negative integers using the default options. Equivalent to decode/3 with an empty options list. Numbers is [] if Id is the empty atom or contains a character that is not in the alphabet. Decoding does not check that Id is a canonical (minimal) encoding: ids rejected by encode/2-3 due to the blocklist still decode successfully, as do ids with extraneous minimum-length padding.',
		argnames is ['Id', 'Numbers'],
		exceptions is [
			'``Id`` is a variable' - instantiation_error,
			'``Id`` is neither a variable nor an atom' - type_error(atom, 'Id')
		]
	]).

	:- public(decode/3).
	:- mode(decode(+text, -list(integer), +list(compound)), one_or_error).
	:- info(decode/3, [
		comment is 'Decodes an identifier back into a list of non-negative integers using the given options. The only recognized option is alphabet(Atom); it must be the same alphabet that was used to encode Id. Numbers is [] if Id is the empty atom or contains a character that is not in the alphabet.',
		argnames is ['Id', 'Numbers', 'Options'],
		exceptions is [
			'``Id`` is a variable' - instantiation_error,
			'``Id`` is neither a variable nor an atom' - type_error(atom, 'Id'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth0/3, remove_duplicates/2, reverse/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	encode(Numbers, Id) :-
		encode(Numbers, Id, []).

	encode(Numbers, Id, UserOptions) :-
		context(Context),
		check(list(non_negative_integer), Numbers, Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(alphabet(Alphabet), Options),
		^^option(min_length(MinLength), Options),
		^^option(blocklist(Blocklist), Options),
		alphabet_chars(Alphabet, AlphabetChars),
		(	Numbers == [] ->
			Id = ''
		;	filter_blocklist(Blocklist, AlphabetChars, FilteredBlocklist),
			encode_numbers(Numbers, AlphabetChars, MinLength, FilteredBlocklist, 0, IdChars),
			chars_text(_Representation_, IdChars, Id)
		).

	decode(Id, Numbers) :-
		decode(Id, Numbers, []).

	decode(Id, Numbers, UserOptions) :-
		context(Context),
		check(_Representation_, Id, Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(alphabet(Alphabet), Options),
		alphabet_chars(Alphabet, AlphabetChars),
		text_chars(_Representation_, Id, IdChars),
		decode_(IdChars, AlphabetChars, Numbers).

	default_option(alphabet('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')).
	default_option(min_length(0)).
	default_option(blocklist([])).

	valid_option(alphabet(Alphabet)) :-
		atom(Alphabet),
		atom_length(Alphabet, Length),
		Length >= 3,
		atom_chars(Alphabet, Chars),
		remove_duplicates(Chars, Unique),
		length(Unique, Length).
	valid_option(blocklist(Blocklist)) :-
		valid(list(atom), Blocklist).
	valid_option(min_length(MinLength)) :-
		integer(MinLength),
		MinLength >= 0,
		MinLength =< 255.

	% ---------------------------------------------------------------
	% shared alphabet setup
	% ---------------------------------------------------------------

	% the alphabet used throughout an encode/3 or decode/3 call is the
	% user-supplied (or default) alphabet shuffled once; this mirrors
	% the reference implementation, where the constructor shuffles the
	% alphabet a single time and reuses that shuffled alphabet for
	% every subsequent operation

	alphabet_chars(Alphabet, Chars) :-
		atom_chars(Alphabet, Chars0),
		shuffle(Chars0, Chars).

	% ---------------------------------------------------------------
	% encoding
	% ---------------------------------------------------------------

	encode_numbers(Numbers, Alphabet, MinLength, Blocklist, Increment, IdChars) :-
		length(Alphabet, Length),
		(	Increment > Length ->
			resource_error(blocklist_retries)
		;	numbers_offset(Numbers, Alphabet, Length, Offset0),
			Offset is (Offset0 + Increment) mod Length,
			rotate(Alphabet, Offset, RotatedAlphabet),
			RotatedAlphabet = [Prefix| _],
			reverse(RotatedAlphabet, WorkingAlphabet),
			encode_loop(Numbers, WorkingAlphabet, PartChars, FinalAlphabet),
			IdChars0 = [Prefix| PartChars],
			pad_id(IdChars0, FinalAlphabet, MinLength, IdChars1),
			(	is_blocked_id(IdChars1, Blocklist) ->
				Increment1 is Increment + 1,
				encode_numbers(Numbers, Alphabet, MinLength, Blocklist, Increment1, IdChars)
			;	IdChars = IdChars1
			)
		).

	% single (last) number: no separator is appended and the alphabet is not
	% reshuffled afterwards, so the alphabet used to encode it is also the
	% alphabet in effect for the subsequent minimum-length padding step
	encode_loop([Number], Alphabet, Chars, Alphabet) :-
		!,
		Alphabet = [_Separator| Rest],
		to_id_chars(Number, Rest, Chars).
	encode_loop([Number| Numbers], Alphabet, Chars, FinalAlphabet) :-
		Alphabet = [Separator| Rest],
		to_id_chars(Number, Rest, NumberChars),
		shuffle(Alphabet, NextAlphabet),
		encode_loop(Numbers, NextAlphabet, RestChars, FinalAlphabet),
		append(NumberChars, [Separator| RestChars], Chars).

	numbers_offset(Numbers, Alphabet, Length, Offset) :-
		length(Numbers, Count),
		offset_fold(Numbers, Alphabet, Length, 0, Count, Sum),
		Offset is Sum mod Length.

	offset_fold([], _, _, _, Sum, Sum).
	offset_fold([Number| Numbers], Alphabet, Length, Index, Acc, Sum) :-
		Position is Number mod Length,
		nth0(Position, Alphabet, Char),
		char_code(Char, Code),
		Acc1 is Acc + Code + Index,
		Index1 is Index + 1,
		offset_fold(Numbers, Alphabet, Length, Index1, Acc1, Sum).

	to_id_chars(Number, Alphabet, Chars) :-
		length(Alphabet, Length),
		to_id_chars(Number, Alphabet, Length, [], Chars).

	to_id_chars(Number, Alphabet, Length, Acc, Chars) :-
		Position is Number mod Length,
		nth0(Position, Alphabet, Char),
		Acc1 = [Char| Acc],
		Rest is Number // Length,
		(	Rest > 0 ->
			to_id_chars(Rest, Alphabet, Length, Acc1, Chars)
		;	Chars = Acc1
		).

	pad_id(IdChars, Alphabet, MinLength, PaddedChars) :-
		length(IdChars, Length),
		(	MinLength =< Length ->
			PaddedChars = IdChars
		;	Alphabet = [Separator| _],
			append(IdChars, [Separator], IdChars1),
			pad_loop(IdChars1, Alphabet, MinLength, PaddedChars)
		).

	pad_loop(IdChars, Alphabet, MinLength, PaddedChars) :-
		length(IdChars, Length),
		Remaining is MinLength - Length,
		(	Remaining =< 0 ->
			PaddedChars = IdChars
		;	shuffle(Alphabet, NextAlphabet),
			length(NextAlphabet, AlphabetLength),
			Take is min(Remaining, AlphabetLength),
			length(TakeChars, Take),
			append(TakeChars, _, NextAlphabet),
			append(IdChars, TakeChars, IdChars1),
			pad_loop(IdChars1, NextAlphabet, MinLength, PaddedChars)
		).

	% ---------------------------------------------------------------
	% decoding
	% ---------------------------------------------------------------

	decode_([], _, []) :-
		!.
	decode_(IdChars, AlphabetChars, Numbers) :-
		(	forall(member(Char, IdChars), member(Char, AlphabetChars)) ->
			IdChars = [Prefix| Rest],
			once(nth0(Offset, AlphabetChars, Prefix)),
			rotate(AlphabetChars, Offset, RotatedAlphabet),
			reverse(RotatedAlphabet, WorkingAlphabet),
			decode_loop(Rest, WorkingAlphabet, Numbers)
		;	Numbers = []
		).

	decode_loop([], _, []) :-
		!.
	decode_loop(IdChars, Alphabet, Numbers) :-
		Alphabet = [Separator| AlphabetWithoutSeparator],
		(	split_first(Separator, IdChars, Chunk, After) ->
			Found = true
		;	Chunk = IdChars, After = [], Found = false
		),
		(	Chunk == [] ->
			% two separators in a row: the rest of the id is minimum-length
			% padding, not encoded data, per the Sqids specification
			Numbers = []
		;	chars_to_number(Chunk, AlphabetWithoutSeparator, Number),
			(	Found == true ->
				shuffle(Alphabet, NextAlphabet),
				decode_loop(After, NextAlphabet, NumbersTail),
				Numbers = [Number| NumbersTail]
			;	Numbers = [Number]
			)
		).

	split_first(Separator, List, Before, After) :-
		append(Before, [Separator| After], List),
		!.

	chars_to_number(Chars, Alphabet, Number) :-
		length(Alphabet, Length),
		chars_to_number(Chars, Alphabet, Length, 0, Number).

	chars_to_number([], _, _, Number, Number).
	chars_to_number([Char| Chars], Alphabet, Length, Acc, Number) :-
		once(nth0(Index, Alphabet, Char)),
		Acc1 is Acc * Length + Index,
		chars_to_number(Chars, Alphabet, Length, Acc1, Number).

	% ---------------------------------------------------------------
	% consistent (non-random) alphabet shuffle
	% ---------------------------------------------------------------

	shuffle(Alphabet, Shuffled) :-
		length(Alphabet, Length),
		(	Length =< 1 ->
			Shuffled = Alphabet
		;	LastIndex is Length - 1,
			shuffle(0, LastIndex, Length, Alphabet, Shuffled)
		).

	shuffle(_, J, _, Chars, Chars) :-
		J =< 0,
		!.
	shuffle(I, J, Length, Chars, Result) :-
		nth0(I, Chars, CharI),
		nth0(J, Chars, CharJ),
		char_code(CharI, CodeI),
		char_code(CharJ, CodeJ),
		R is (I * J + CodeI + CodeJ) mod Length,
		swap(I, R, Chars, Chars1),
		I1 is I + 1,
		J1 is J - 1,
		shuffle(I1, J1, Length, Chars1, Result).

	swap(I, I, List, List) :- !.
	swap(I, J, List, NewList) :-
		nth0(I, List, ElementI),
		nth0(J, List, ElementJ),
		set_nth0(I, List, ElementJ, List1),
		set_nth0(J, List1, ElementI, NewList).

	% ---------------------------------------------------------------
	% blocklist filtering and matching
	% ---------------------------------------------------------------

	% per the specification, the blocklist used at encoding time is derived
	% from the user-supplied (or default) blocklist by: lower-casing every
	% word, dropping words shorter than three characters, and dropping
	% words that contain a character not in the alphabet
	filter_blocklist(Blocklist, AlphabetChars, Filtered) :-
		chars_downcase(AlphabetChars, AlphabetLower0),
		remove_duplicates(AlphabetLower0, AlphabetLower),
		findall(
			WordLower,
			(	member(Word, Blocklist),
				ascii_downcase_atom(Word, WordLower),
				atom_length(WordLower, Length),
				Length >= 3,
				atom_chars(WordLower, WordChars),
				forall(member(Char, WordChars), member(Char, AlphabetLower))
			),
			Filtered0
		),
		remove_duplicates(Filtered0, Filtered).

	is_blocked_id(IdChars, Blocklist) :-
		atom_chars(IdAtom, IdChars),
		ascii_downcase_atom(IdAtom, IdLower),
		atom_length(IdLower, IdLength),
		member(WordLower, Blocklist),
		atom_length(WordLower, WordLength),
		WordLength =< IdLength,
		blocked_match(IdLower, IdLength, WordLower, WordLength),
		!.

	% short words (or short ids) must match exactly
	blocked_match(IdLower, IdLength, WordLower, WordLength) :-
		( IdLength =< 3 ; WordLength =< 3 ),
		!,
		IdLower == WordLower.
	% leetspeak variants (containing a digit) only match at the start or end
	blocked_match(IdLower, _, WordLower, _) :-
		has_ascii_digit(WordLower),
		!,
		(	sub_atom(IdLower, 0, _, _, WordLower)
		;	sub_atom(IdLower, _, _, 0, WordLower)
		),
		!.
	% otherwise, match anywhere in the id
	blocked_match(IdLower, _, WordLower, _) :-
		sub_atom(IdLower, _, _, _, WordLower),
		!.

	% ---------------------------------------------------------------
	% auxiliary predicates
	% ---------------------------------------------------------------

	set_nth0(0, [_| Rest], Element, [Element| Rest]) :-
		!.
	set_nth0(N, [Head| Rest], Element, [Head| Rest1]) :-
		N > 0,
		N1 is N - 1,
		set_nth0(N1, Rest, Element, Rest1).

	rotate(List, Offset, Rotated) :-
		length(Head, Offset),
		append(Head, Tail, List),
		append(Tail, Head, Rotated).

	% ASCII-only case folding and digit checking, used for blocklist
	% matching; implemented locally for portability and because the
	% default alphabet and the reference blocklist are themselves ASCII

	ascii_downcase_atom(Atom, Lower) :-
		atom_chars(Atom, Chars),
		chars_downcase(Chars, LowerChars),
		atom_chars(Lower, LowerChars).

	chars_downcase([], []).
	chars_downcase([Char| Chars], [Lower| LowerChars]) :-
		ascii_downcase_char(Char, Lower),
		chars_downcase(Chars, LowerChars).

	ascii_downcase_char(Char, Lower) :-
		char_code(Char, Code),
		(	Code >= 0'A, Code =< 0'Z ->
			LowerCode is Code + 32,
			char_code(Lower, LowerCode)
		;	Lower = Char
		).

	has_ascii_digit(Atom) :-
		atom_chars(Atom, Chars),
		member(Char, Chars),
		is_ascii_digit_char(Char),
		!.

	is_ascii_digit_char(Char) :-
		char_code(Char, Code),
		Code >= 0'0,
		Code =< 0'9.

	% text representation conversions

	text_chars(atom, Atom, Chars) :-
		atom_chars(Atom, Chars).
	text_chars(chars, Chars, Chars).
	text_chars(codes, Codes, Chars) :-
		codes_chars(Codes, Chars).

	chars_text(atom, Chars, Atom) :-
		atom_chars(Atom, Chars).
	chars_text(chars, Chars, Chars).
	chars_text(codes, Chars, Codes) :-
		chars_codes(Chars, Codes).

	codes_chars([], []).
	codes_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_chars(Codes, Chars).

	chars_codes([], []).
	chars_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_codes(Chars, Codes).

:- end_object.


:- object(sqids,
	extends(sqids(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-25,
		comment is 'Encoding and decoding of short, obfuscated, URL-safe identifiers from lists of non-negative integers, per the Sqids specification, using an atom representation.',
		see_also is [sqids(_), ids, cuid2, ksuid, nanoid, snowflakeid, typeid, ulid, uuid]
	]).

:- end_object.
