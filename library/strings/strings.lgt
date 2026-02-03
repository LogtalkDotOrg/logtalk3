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


:- object(string(_Representation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-03,
		comment is 'String manipulation predicates supporting different string representations.',
		parameters is [
			'Representation' - 'String representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		]
	]).

	:- public(atom_string/2).
	:- mode(atom_string(+atom, ?text), zero_or_one).
	:- mode(atom_string(-atom, +text), zero_or_one).
	:- info(atom_string/2, [
		comment is 'Converts between an atom and a string.',
		argnames is ['Atom', 'String']
	]).

	:- public(number_string/2).
	:- mode(number_string(+number, ?text), zero_or_one).
	:- mode(number_string(-number, +text), zero_or_one).
	:- info(number_string/2, [
		comment is 'Converts between a number and a string. Fails if the string does not represent a valid number.',
		argnames is ['Number', 'String']
	]).

	:- public(string_chars/2).
	:- mode(string_chars(+text, ?list(character)), zero_or_one).
	:- mode(string_chars(-text, +list(character)), zero_or_one).
	:- info(string_chars/2, [
		comment is 'Converts between a string and a list of characters.',
		argnames is ['String', 'Chars']
	]).

	:- public(string_codes/2).
	:- mode(string_codes(+text, ?list(character_code)), zero_or_one).
	:- mode(string_codes(-text, +list(character_code)), zero_or_one).
	:- info(string_codes/2, [
		comment is 'Converts between a string and a list of character codes.',
		argnames is ['String', 'Codes']
	]).

	:- public(string_concat/3).
	:- mode(string_concat(+text, +text, ?text), zero_or_one).
	:- mode(string_concat(?text, ?text, +text), zero_or_more).
	:- info(string_concat/3, [
		comment is 'Concatenates two strings.',
		argnames is ['String1', 'String2', 'String3']
	]).

	:- public(string_length/2).
	:- mode(string_length(+text, ?integer), zero_or_one).
	:- info(string_length/2, [
		comment is 'Returns the length of a string.',
		argnames is ['String', 'Length']
	]).

	:- public(sub_string/5).
	:- mode(sub_string(+text, ?integer, ?integer, ?integer, ?text), zero_or_more).
	:- info(sub_string/5, [
		comment is 'Extracts a substring from a string.',
		argnames is ['String', 'Before', 'Length', 'After', 'SubString']
	]).

	:- public(string_upper/2).
	:- mode(string_upper(+text, ?text), zero_or_one).
	:- info(string_upper/2, [
		comment is 'Converts a string to uppercase (ASCII only).',
		argnames is ['String', 'UpperString']
	]).

	:- public(string_lower/2).
	:- mode(string_lower(+text, ?text), zero_or_one).
	:- info(string_lower/2, [
		comment is 'Converts a string to lowercase (ASCII only).',
		argnames is ['String', 'LowerString']
	]).

	:- public(split_string/4).
	:- mode(split_string(+text, +text, +text, -list(text)), one).
	:- info(split_string/4, [
		comment is 'Decomposes ``String`` into ``SubStrings`` according to separators ``SepChars`` and padding characters ``PadChars``. The string is split at the separators, and any padding characters around the resulting sub-strings are removed. Characters in both ``SepChars`` and ``PadChars`` are treated as separators where sequences count as one separator, and are ignored at string boundaries.',
		argnames is ['String', 'SepChars', 'PadChars', 'SubStrings']
	]).

	:- public(atomics_to_string/2).
	:- mode(atomics_to_string(++list(atomic), -text), one).
	:- info(atomics_to_string/2, [
		comment is 'Concatenates the atomic terms in ``List`` into ``String``. The list may contain numbers, atoms, and strings (in the current representation).',
		argnames is ['List', 'String']
	]).

	:- public(atomics_to_string/3).
	:- mode(atomics_to_string(++list(atomic), +text, -text), one).
	:- info(atomics_to_string/3, [
		comment is 'Concatenates the atomic terms in ``List`` into ``String``, with ``Separator`` inserted between each element. The list may contain numbers, atoms, and strings (in the current representation).',
		argnames is ['List', 'Separator', 'String']
	]).

	:- public(trim/2).
	:- mode(trim(+text, -text), one).
	:- info(trim/2, [
		comment is 'Trims string by deleting all leading and trailing whitespace.',
		argnames is ['String', 'Trimmed']
	]).

	:- public(trim/3).
	:- mode(trim(+text, +text, -text), one).
	:- info(trim/3, [
		comment is 'Trims string by deleting all occurrences of the characters in ``Elements`` from the beginning and end of the string.',
		argnames is ['String', 'Elements', 'Trimmed']
	]).

	:- public(trim_left/2).
	:- mode(trim_left(+text, -text), one).
	:- info(trim_left/2, [
		comment is 'Trims string by deleting all leading whitespace.',
		argnames is ['String', 'Trimmed']
	]).

	:- public(trim_left/3).
	:- mode(trim_left(+text, +text, -text), one).
	:- info(trim_left/3, [
		comment is 'Trims string by deleting all occurrences of the characters in ``Elements`` from the beginning of the string.',
		argnames is ['String', 'Elements', 'Trimmed']
	]).

	:- public(trim_right/2).
	:- mode(trim_right(+text, -text), one).
	:- info(trim_right/2, [
		comment is 'Trims string by deleting all trailing whitespace.',
		argnames is ['String', 'Trimmed']
	]).

	:- public(trim_right/3).
	:- mode(trim_right(+text, +text, -text), one).
	:- info(trim_right/3, [
		comment is 'Trims string by deleting all occurrences of the characters in ``Elements`` from the end of the string.',
		argnames is ['String', 'Elements', 'Trimmed']
	]).

	:- uses(list, [
		append/3, length/2, reverse/2
	]).

	% atom_string/2

	atom_string(Atom, String) :-
		atom_string(_Representation_, Atom, String).

	atom_string(atom, Atom, Atom) :-
		atom(Atom).
	atom_string(chars, Atom, Chars) :-
		(	atom(Atom) ->
			atom_chars(Atom, Chars)
		;	atom_chars(Atom, Chars)
		).
	atom_string(codes, Atom, Codes) :-
		(	atom(Atom) ->
			atom_codes(Atom, Codes)
		;	atom_codes(Atom, Codes)
		).

	% number_string/2

	number_string(Number, String) :-
		number_string(_Representation_, Number, String).

	number_string(atom, Number, Atom) :-
		(	number(Number) ->
			number_codes(Number, Codes),
			atom_codes(Atom, Codes)
		;	atom_codes(Atom, Codes),
			catch(number_codes(Number, Codes), _, fail)
		).
	number_string(chars, Number, Chars) :-
		(	number(Number) ->
			number_codes(Number, Codes),
			codes_to_chars(Codes, Chars)
		;	chars_to_codes(Chars, Codes),
			catch(number_codes(Number, Codes), _, fail)
		).
	number_string(codes, Number, Codes) :-
		catch(number_codes(Number, Codes), _, fail).

	% string_chars/2

	string_chars(String, Chars) :-
		string_chars(_Representation_, String, Chars).

	string_chars(atom, Atom, Chars) :-
		atom_chars(Atom, Chars).
	string_chars(chars, Chars, Chars).
	string_chars(codes, Codes, Chars) :-
		(	nonvar(Codes) ->
			codes_to_chars(Codes, Chars)
		;	chars_to_codes(Chars, Codes)
		).

	% string_codes/2

	string_codes(String, Codes) :-
		string_codes(_Representation_, String, Codes).

	string_codes(atom, Atom, Codes) :-
		atom_codes(Atom, Codes).
	string_codes(chars, Chars, Codes) :-
		(	nonvar(Chars) ->
			chars_to_codes(Chars, Codes)
		;	codes_to_chars(Codes, Chars)
		).
	string_codes(codes, Codes, Codes).

	% string_concat/3

	string_concat(String1, String2, String3) :-
		string_concat(_Representation_, String1, String2, String3).

	string_concat(atom, Atom1, Atom2, Atom3) :-
		atom_concat(Atom1, Atom2, Atom3).
	string_concat(chars, Chars1, Chars2, Chars3) :-
		append(Chars1, Chars2, Chars3).
	string_concat(codes, Codes1, Codes2, Codes3) :-
		append(Codes1, Codes2, Codes3).

	% string_length/2

	string_length(String, Length) :-
		string_length(_Representation_, String, Length).

	string_length(atom, Atom, Length) :-
		atom_length(Atom, Length).
	string_length(chars, Chars, Length) :-
		length(Chars, Length).
	string_length(codes, Codes, Length) :-
		length(Codes, Length).

	% sub_string/5

	sub_string(String, Before, Length, After, SubString) :-
		sub_string(_Representation_, String, Before, Length, After, SubString).

	sub_string(atom, Atom, Before, Length, After, SubAtom) :-
		sub_atom(Atom, Before, Length, After, SubAtom).
	sub_string(chars, Chars, Before, Length, After, SubChars) :-
		atom_chars(Atom, Chars),
		sub_atom(Atom, Before, Length, After, SubAtom),
		atom_chars(SubAtom, SubChars).
	sub_string(codes, Codes, Before, Length, After, SubCodes) :-
		atom_codes(Atom, Codes),
		sub_atom(Atom, Before, Length, After, SubAtom),
		atom_codes(SubAtom, SubCodes).

	% string_upper/2

	string_upper(String, UpperString) :-
		string_upper(_Representation_, String, UpperString).

	string_upper(atom, Atom, UpperAtom) :-
		atom_codes(Atom, Codes),
		upcase_codes(Codes, UpperCodes),
		atom_codes(UpperAtom, UpperCodes).
	string_upper(chars, Chars, UpperChars) :-
		chars_to_codes(Chars, Codes),
		upcase_codes(Codes, UpperCodes),
		codes_to_chars(UpperCodes, UpperChars).
	string_upper(codes, Codes, UpperCodes) :-
		upcase_codes(Codes, UpperCodes).

	% string_lower/2

	string_lower(String, LowerString) :-
		string_lower(_Representation_, String, LowerString).

	string_lower(atom, Atom, LowerAtom) :-
		atom_codes(Atom, Codes),
		downcase_codes(Codes, LowerCodes),
		atom_codes(LowerAtom, LowerCodes).
	string_lower(chars, Chars, LowerChars) :-
		chars_to_codes(Chars, Codes),
		downcase_codes(Codes, LowerCodes),
		codes_to_chars(LowerCodes, LowerChars).
	string_lower(codes, Codes, LowerCodes) :-
		downcase_codes(Codes, LowerCodes).

	% split_string/4

	split_string(String, SepChars, PadChars, SubStrings) :-
		split_string(_Representation_, String, SepChars, PadChars, SubStrings).

	split_string(atom, String, SepChars, PadChars, SubStrings) :-
		atom_codes(String, StringCodes),
		atom_codes(SepChars, SepCodes),
		atom_codes(PadChars, PadCodes),
		split_codes(StringCodes, SepCodes, PadCodes, SubStringsCodes),
		codes_to_atoms(SubStringsCodes, SubStrings).
	split_string(chars, String, SepChars, PadChars, SubStrings) :-
		chars_to_codes(String, StringCodes),
		chars_to_codes(SepChars, SepCodes),
		chars_to_codes(PadChars, PadCodes),
		split_codes(StringCodes, SepCodes, PadCodes, SubStringsCodes),
		codes_to_chars_list(SubStringsCodes, SubStrings).
	split_string(codes, String, SepChars, PadChars, SubStrings) :-
		split_codes(String, SepChars, PadChars, SubStrings).

	% atomics_to_string/2

	atomics_to_string(List, String) :-
		atomics_to_string(_Representation_, List, String).

	atomics_to_string(atom, List, Atom) :-
		atomics_to_codes(List, Codes),
		atom_codes(Atom, Codes).
	atomics_to_string(chars, List, Chars) :-
		atomics_to_codes(List, Codes),
		codes_to_chars(Codes, Chars).
	atomics_to_string(codes, List, Codes) :-
		atomics_to_codes(List, Codes).

	% atomics_to_string/3

	atomics_to_string(List, Separator, String) :-
		atomics_to_string(_Representation_, List, Separator, String).

	atomics_to_string(atom, List, Separator, Atom) :-
		atom_codes(Separator, SeparatorCodes),
		atomics_to_codes_with_glue(List, SeparatorCodes, Codes),
		atom_codes(Atom, Codes).
	atomics_to_string(chars, List, Separator, Chars) :-
		chars_to_codes(Separator, SeparatorCodes),
		atomics_to_codes_with_glue(List, SeparatorCodes, Codes),
		codes_to_chars(Codes, Chars).
	atomics_to_string(codes, List, Separator, Codes) :-
		atomics_to_codes_with_glue(List, Separator, Codes).

	% auxiliary predicates

	upcase_codes([], []).
	upcase_codes([Code| Codes], [UpperCode| UpperCodes]) :-
		(	Code >= 0'a, Code =< 0'z ->
			UpperCode is Code - 32
		;	UpperCode = Code
		),
		upcase_codes(Codes, UpperCodes).

	downcase_codes([], []).
	downcase_codes([Code| Codes], [LowerCode| LowerCodes]) :-
		(	Code >= 0'A, Code =< 0'Z ->
			LowerCode is Code + 32
		;	LowerCode = Code
		),
		downcase_codes(Codes, LowerCodes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	codes_to_atoms([], []).
	codes_to_atoms([Codes| Rest], [Atom| Atoms]) :-
		atom_codes(Atom, Codes),
		codes_to_atoms(Rest, Atoms).

	codes_to_chars_list([], []).
	codes_to_chars_list([Codes| Rest], [Chars| CharsList]) :-
		codes_to_chars(Codes, Chars),
		codes_to_chars_list(Rest, CharsList).

	% Convert a list of atomics to codes (concatenated)
	atomics_to_codes([], []).
	atomics_to_codes([Atomic| Atomics], Codes) :-
		atomic_to_codes(Atomic, AtomicCodes),
		atomics_to_codes(Atomics, RestCodes),
		append(AtomicCodes, RestCodes, Codes).

	% Convert a list of atomics to codes with glue between elements
	atomics_to_codes_with_glue([], _, []).
	atomics_to_codes_with_glue([Atomic], _, Codes) :-
		atomic_to_codes(Atomic, Codes).
	atomics_to_codes_with_glue([Atomic1, Atomic2| Atomics], SeparatorCodes, Codes) :-
		atomic_to_codes(Atomic1, Codes1),
		append(Codes1, SeparatorCodes, Codes1WithSeparator),
		atomics_to_codes_with_glue([Atomic2| Atomics], SeparatorCodes, RestCodes),
		append(Codes1WithSeparator, RestCodes, Codes).

	% Convert a single atomic to codes
	atomic_to_codes(Atomic, Codes) :-
		(	atom(Atomic) ->
			atom_codes(Atomic, Codes)
		;	number(Atomic) ->
			number_codes(Atomic, Codes)
		;	% Assume it's already a list of codes or chars - convert to codes
			(	Atomic = [H|_], atom(H) ->
				% It's a list of chars
				chars_to_codes(Atomic, Codes)
			;	% It's already a list of codes
				Codes = Atomic
			)
		).

	% split_codes/4 - split a list of codes according to separators and padding
	% Characters in both SepCodes and PadCodes are treated as separators
	% where sequences count as one separator and are ignored at boundaries
	split_codes(StringCodes, SepCodes, PadCodes, SubStrings) :-
		% Find characters that are in both Sep and Pad (special separators)
		intersection_codes(SepCodes, PadCodes, BothCodes),
		% Handle special case: no separators (just padding trim)
		(	SepCodes == [] ->
			strip_padding(StringCodes, PadCodes, Stripped),
			(	Stripped == [] ->
				SubStrings = []
			;	SubStrings = [Stripped]
			)
		;	% Split the string, then strip padding from each substring
			split_at_separators(StringCodes, SepCodes, PadCodes, BothCodes, RawSubStrings),
			strip_all_padding(RawSubStrings, PadCodes, SubStrings)
		).

	% Helper to find intersection of two code lists
	intersection_codes([], _, []).
	intersection_codes([Code| Codes], List2, Result) :-
		(	member_code(Code, List2) ->
			Result = [Code| Rest]
		;	Result = Rest
		),
		intersection_codes(Codes, List2, Rest).

	member_code(Code, [Code| _]) :- !.
	member_code(Code, [_| Codes]) :-
		member_code(Code, Codes).

	% Strip padding from all substrings in a list
	strip_all_padding([], _, []).
	strip_all_padding([Sub| Subs], PadCodes, [Stripped| StrippedSubs]) :-
		strip_padding(Sub, PadCodes, Stripped),
		strip_all_padding(Subs, PadCodes, StrippedSubs).

	% Split string at separator positions
	% BothCodes characters are treated specially - sequences count as one separator
	% and they are ignored at boundaries
	split_at_separators(StringCodes, SepCodes, PadCodes, BothCodes, SubStrings) :-
		% First, skip any leading "both" characters
		skip_both_chars(StringCodes, BothCodes, Trimmed),
		split_at_separators_aux(Trimmed, SepCodes, PadCodes, BothCodes, [], no, SubStrings).

	% The sixth argument tracks whether we just passed a separator (need trailing empty)
	split_at_separators_aux([], _, _, _, Acc, AfterSep, SubStrings) :-
		(	Acc == [] ->
			(	AfterSep == yes ->
				% After a separator with nothing following - add empty string
				SubStrings = [[]]
			;	SubStrings = []
			)
		;	reverse(Acc, SubString),
			SubStrings = [SubString]
		).
	split_at_separators_aux([Code| Codes], SepCodes, PadCodes, BothCodes, Acc, _AfterSep, SubStrings) :-
		(	% Check if it's a "both" character (in both Sep and Pad)
			member_code(Code, BothCodes) ->
			% Skip sequence of "both" characters
			skip_both_chars(Codes, BothCodes, RestCodes),
			(	Acc == [] ->
				% At beginning or after another separator - skip
				split_at_separators_aux(RestCodes, SepCodes, PadCodes, BothCodes, [], no, SubStrings)
			;	% In middle - treat as separator
				reverse(Acc, SubString),
				(	RestCodes == [] ->
					% At end - just add accumulated
					SubStrings = [SubString]
				;	SubStrings = [SubString| RestSubStrings],
					split_at_separators_aux(RestCodes, SepCodes, PadCodes, BothCodes, [], no, RestSubStrings)
				)
			)
		;	% Check if it's a separator (but not a "both" character)
			member_code(Code, SepCodes) ->
			% Split here - this always produces a substring (possibly empty)
			reverse(Acc, SubString),
			SubStrings = [SubString| RestSubStrings],
			split_at_separators_aux(Codes, SepCodes, PadCodes, BothCodes, [], yes, RestSubStrings)
		;	% Regular character - accumulate
			split_at_separators_aux(Codes, SepCodes, PadCodes, BothCodes, [Code| Acc], no, SubStrings)
		).

	% Skip consecutive "both" characters
	skip_both_chars([], _, []).
	skip_both_chars([Code| Codes], BothCodes, Rest) :-
		(	member_code(Code, BothCodes) ->
			skip_both_chars(Codes, BothCodes, Rest)
		;	Rest = [Code| Codes]
		).

	% Strip padding from both ends
	strip_padding(Codes, PadCodes, Stripped) :-
		strip_leading_padding(Codes, PadCodes, Temp),
		reverse(Temp, TempRev),
		strip_leading_padding(TempRev, PadCodes, StrippedRev),
		reverse(StrippedRev, Stripped).

	% Strip leading padding characters
	strip_leading_padding([], _, []).
	strip_leading_padding([Code| Codes], PadCodes, Rest) :-
		(	member_code(Code, PadCodes) ->
			strip_leading_padding(Codes, PadCodes, Rest)
		;	Rest = [Code| Codes]
		).

	% Strip trailing padding characters
	strip_trailing_padding(Codes, PadCodes, Stripped) :-
		reverse(Codes, Temp),
		strip_leading_padding(Temp, PadCodes, TempRev),
		reverse(TempRev, Stripped).

	% Trimming

	trim(String, Trimmed) :-
		trim_(_Representation_, String, Trimmed).

	trim_(atom, String, Trimmed) :-
		atom_codes(String, Codes),
		strip_padding(Codes, [9, 32], TrimmedCodes),
		atom_codes(Trimmed, TrimmedCodes).
	trim_(chars, String, Trimmed) :-
		chars_to_codes(String, Codes),
		strip_padding(Codes, [9, 32], TrimmedCodes),
		codes_to_chars(TrimmedCodes, Trimmed).
	trim_(codes, String, Trimmed) :-
		strip_padding(String, [9, 32], Trimmed).

	trim(String, Elements, Trimmed) :-
		trim_(_Representation_, String, Elements, Trimmed).

	trim_(atom, String, Elements, Trimmed) :-
		atom_codes(String, Codes),
		atom_codes(Elements, ElementsCodes),
		strip_padding(Codes, ElementsCodes, TrimmedCodes),
		atom_codes(Trimmed, TrimmedCodes).
	trim_(chars, String, Elements, Trimmed) :-
		chars_to_codes(String, Codes),
		chars_to_codes(Elements, ElementsCodes),
		strip_padding(Codes, ElementsCodes, TrimmedCodes),
		codes_to_chars(TrimmedCodes, Trimmed).
	trim_(codes, String, Elements, Trimmed) :-
		strip_padding(String, Elements, Trimmed).

	trim_left(String, Trimmed) :-
		trim_left_(_Representation_, String, Trimmed).

	trim_left_(atom, String, Trimmed) :-
		atom_codes(String, Codes),
		strip_leading_padding(Codes, [9, 32], TrimmedCodes),
		atom_codes(Trimmed, TrimmedCodes).
	trim_left_(chars, String, Trimmed) :-
		chars_to_codes(String, Codes),
		strip_leading_padding(Codes, [9, 32], TrimmedCodes),
		codes_to_chars(TrimmedCodes, Trimmed).
	trim_left_(codes, String, Trimmed) :-
		strip_leading_padding(String, [9, 32], Trimmed).

	trim_left(String, Elements, Trimmed) :-
		trim_left_(_Representation_, String, Elements, Trimmed).

	trim_left_(atom, String, Elements, Trimmed) :-
		atom_codes(String, Codes),
		atom_codes(Elements, ElementsCodes),
		strip_leading_padding(Codes, ElementsCodes, TrimmedCodes),
		atom_codes(Trimmed, TrimmedCodes).
	trim_left_(chars, String, Elements, Trimmed) :-
		chars_to_codes(String, Codes),
		chars_to_codes(Elements, ElementsCodes),
		strip_leading_padding(Codes, ElementsCodes, TrimmedCodes),
		codes_to_chars(TrimmedCodes, Trimmed).
	trim_left_(codes, String, Elements, Trimmed) :-
		strip_leading_padding(String, Elements, Trimmed).

	trim_right(String, Trimmed) :-
		trim_right_(_Representation_, String, Trimmed).

	trim_right_(atom, String, Trimmed) :-
		atom_codes(String, Codes),
		strip_trailing_padding(Codes, [9, 32], TrimmedCodes),
		atom_codes(Trimmed, TrimmedCodes).
	trim_right_(chars, String, Trimmed) :-
		chars_to_codes(String, Codes),
		strip_trailing_padding(Codes, [9, 32], TrimmedCodes),
		codes_to_chars(TrimmedCodes, Trimmed).
	trim_right_(codes, String, Trimmed) :-
		strip_trailing_padding(String, [9, 32], Trimmed).

	trim_right(String, Elements, Trimmed) :-
		trim_right_(_Representation_, String, Elements, Trimmed).

	trim_right_(atom, String, Elements, Trimmed) :-
		atom_codes(String, Codes),
		atom_codes(Elements, ElementsCodes),
		strip_trailing_padding(Codes, ElementsCodes, TrimmedCodes),
		atom_codes(Trimmed, TrimmedCodes).
	trim_right_(chars, String, Elements, Trimmed) :-
		chars_to_codes(String, Codes),
		chars_to_codes(Elements, ElementsCodes),
		strip_trailing_padding(Codes, ElementsCodes, TrimmedCodes),
		codes_to_chars(TrimmedCodes, Trimmed).
	trim_right_(codes, String, Elements, Trimmed) :-
		strip_trailing_padding(String, Elements, Trimmed).

:- end_object.
