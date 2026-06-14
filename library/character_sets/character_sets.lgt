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


:- category(character_set,
	implements(character_set_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Shared implementation support category for character set objects.'
	]).

	:- protected(valid_unicode_scalar/1).
	:- mode(valid_unicode_scalar(+integer), zero_or_one).
	:- info(valid_unicode_scalar/1, [
		comment is 'True if the argument is a valid Unicode scalar value.',
		argnames is ['Code']
	]).

	:- protected(continuation_byte/1).
	:- mode(continuation_byte(+integer), zero_or_one).
	:- info(continuation_byte/1, [
		comment is 'True if the argument is a valid UTF continuation byte.',
		argnames is ['Byte']
	]).

	:- protected(high_surrogate/1).
	:- mode(high_surrogate(+integer), zero_or_one).
	:- info(high_surrogate/1, [
		comment is 'True if the argument is a UTF-16 high surrogate code point.',
		argnames is ['Code']
	]).

	:- protected(low_surrogate/1).
	:- mode(low_surrogate(+integer), zero_or_one).
	:- info(low_surrogate/1, [
		comment is 'True if the argument is a UTF-16 low surrogate code point.',
		argnames is ['Code']
	]).

	:- protected(word_bytes/4).
	:- mode(word_bytes(+atom, +integer, -integer, -integer), one).
	:- info(word_bytes/4, [
		comment is 'Converts a 16-bit word into two bytes using the given byte order.',
		argnames is ['Endian', 'Word', 'Byte1', 'Byte2']
	]).

	:- protected(dword_bytes/6).
	:- mode(dword_bytes(+atom, +integer, -integer, -integer, -integer, -integer), one).
	:- info(dword_bytes/6, [
		comment is 'Converts a 32-bit word into four bytes using the given byte order.',
		argnames is ['Endian', 'Word', 'Byte1', 'Byte2', 'Byte3', 'Byte4']
	]).

	:- protected(bytes_word/4).
	:- mode(bytes_word(+atom, +integer, +integer, -integer), one).
	:- info(bytes_word/4, [
		comment is 'Converts two bytes into a 16-bit word using the given byte order.',
		argnames is ['Endian', 'Byte1', 'Byte2', 'Word']
	]).

	:- protected(bytes_dword/6).
	:- mode(bytes_dword(+atom, +integer, +integer, +integer, +integer, -integer), one).
	:- info(bytes_dword/6, [
		comment is 'Converts four bytes into a 32-bit word using the given byte order.',
		argnames is ['Endian', 'Byte1', 'Byte2', 'Byte3', 'Byte4', 'Word']
	]).

	valid_unicode_scalar(Code) :-
		integer(Code),
		Code >= 0,
		Code =< 0x10FFFF,
		(	Code < 0xD800 ->
			true
		;	Code > 0xDFFF
		).

	continuation_byte(Byte) :-
		Byte >= 0x80,
		Byte =< 0xBF.

	high_surrogate(Code) :-
		Code >= 0xD800,
		Code =< 0xDBFF.

	low_surrogate(Code) :-
		Code >= 0xDC00,
		Code =< 0xDFFF.

	word_bytes(little_endian, Word, Byte1, Byte2) :-
		Byte1 is Word /\ 0xFF,
		Byte2 is (Word >> 8) /\ 0xFF.
	word_bytes(big_endian, Word, Byte1, Byte2) :-
		Byte1 is (Word >> 8) /\ 0xFF,
		Byte2 is Word /\ 0xFF.

	dword_bytes(little_endian, Word, Byte1, Byte2, Byte3, Byte4) :-
		Byte1 is Word /\ 0xFF,
		Byte2 is (Word >> 8) /\ 0xFF,
		Byte3 is (Word >> 16) /\ 0xFF,
		Byte4 is (Word >> 24) /\ 0xFF.
	dword_bytes(big_endian, Word, Byte1, Byte2, Byte3, Byte4) :-
		Byte1 is (Word >> 24) /\ 0xFF,
		Byte2 is (Word >> 16) /\ 0xFF,
		Byte3 is (Word >> 8) /\ 0xFF,
		Byte4 is Word /\ 0xFF.

	bytes_word(little_endian, Byte1, Byte2, Word) :-
		Word is Byte1 \/ (Byte2 << 8).
	bytes_word(big_endian, Byte1, Byte2, Word) :-
		Word is (Byte1 << 8) \/ Byte2.

	bytes_dword(little_endian, Byte1, Byte2, Byte3, Byte4, Word) :-
		Word is Byte1 \/ (Byte2 << 8) \/ (Byte3 << 16) \/ (Byte4 << 24).
	bytes_dword(big_endian, Byte1, Byte2, Byte3, Byte4, Word) :-
		Word is (Byte1 << 24) \/ (Byte2 << 16) \/ (Byte3 << 8) \/ Byte4.

:- end_category.


:- category(single_byte_character_set(_MaxCode_),
	extends(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Single-byte character set implementation parameterized by the maximum valid code.',
		parnames is ['MaxCode']
	]).

	codes_to_bytes([], []).
	codes_to_bytes([Code| Codes], [Code| Bytes]) :-
		Code >= 0,
		Code =< _MaxCode_,
		codes_to_bytes(Codes, Bytes).

	bytes_to_codes([], []).
	bytes_to_codes([Byte| Bytes], [Byte| Codes]) :-
		Byte >= 0,
		Byte =< _MaxCode_,
		bytes_to_codes(Bytes, Codes).

:- end_category.


:- category(mapped_single_byte_character_set,
	extends(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Single-byte character set implementation parameterized by sparse byte-to-code mapping facts and undefined bytes.'
	]).

	:- protected(mapping/2).
	:- mode(mapping(?integer, ?integer), zero_or_more).
	:- info(mapping/2, [
		comment is 'Returns, by backtracking, declared byte-to-code mappings.',
		argnames is ['Byte', 'Code']
	]).
	:- protected(undefined/2).
	:- mode(undefined(?integer, ?integer), zero_or_more).
	:- info(undefined/2, [
		comment is 'Returns, by backtracking, declared undefined byte values.',
		argnames is ['Byte', 'Code']
	]).

	codes_to_bytes([], []).
	codes_to_bytes([Code| Codes], [Byte| Bytes]) :-
		(	::mapping(Byte, Code) ->
			true
		;	Byte = Code,
			\+ ::mapping(Byte, _),
			\+ ::undefined(Byte, _)
		),
		codes_to_bytes(Codes, Bytes).

	bytes_to_codes([], []).
	bytes_to_codes([Byte| Bytes], [Code| Codes]) :-
		(	::mapping(Byte, Code) ->
			true
		;	Code = Byte,
			\+ ::mapping(Byte, _),
			\+ ::undefined(Byte, _)
		),
		bytes_to_codes(Bytes, Codes).

:- end_category.


:- object(us_ascii,
	imports(single_byte_character_set(0x7F))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'US-ASCII character set encoder and decoder.'
	]).

	preferred_mime_name('US-ASCII').

	name('US-ASCII').

	alias('iso-ir-6').
	alias('ANSI_X3.4-1968').
	alias('ANSI_X3.4-1986').
	alias('ISO_646.irv:1991').
	alias('ISO646-US').
	alias(us).
	alias('IBM367').
	alias(cp367).
	alias('csASCII').

	mibenum(3).

:- end_object.
