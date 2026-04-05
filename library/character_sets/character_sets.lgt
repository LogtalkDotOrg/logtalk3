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
		(   ::mapping(Byte, Code) ->
			true
		;   Byte = Code,
			\+ ::mapping(Byte, _),
			\+ ::undefined(Byte, _)
		),
		codes_to_bytes(Codes, Bytes).

	bytes_to_codes([], []).
	bytes_to_codes([Byte| Bytes], [Code| Codes]) :-
		(   ::mapping(Byte, Code) ->
			true
		;   Code = Byte,
			\+ ::mapping(Byte, _),
			\+ ::undefined(Byte, _)
		),
		bytes_to_codes(Bytes, Codes).

:- end_category.


:- object(utf_8_character_set,
	imports(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-8 character set implementation.'
	]).

	preferred_mime_name(_) :-
		fail.

	name(_) :-
		fail.

	alias(_) :-
		fail.

	mibenum(_) :-
		fail.

	codes_to_bytes(Codes, Bytes) :-
		codes_to_bytes(Codes, Bytes, []).

	bytes_to_codes(Bytes, Codes) :-
		bytes_to_codes_(Bytes, Codes).

	codes_to_bytes([], Bytes, Bytes).
	codes_to_bytes([Code| Codes], Bytes0, Bytes) :-
		^^valid_unicode_scalar(Code),
		code_to_bytes(Code, Bytes0, Bytes1),
		codes_to_bytes(Codes, Bytes1, Bytes).

	code_to_bytes(Code, [Code| Bytes], Bytes) :-
		Code =< 0x7F,
		!.
	code_to_bytes(Code, [Byte1, Byte2| Bytes], Bytes) :-
		Code =< 0x7FF,
		!,
		Byte1 is 0xC0 \/ (Code >> 6),
		Byte2 is 0x80 \/ (Code /\ 0x3F).
	code_to_bytes(Code, [Byte1, Byte2, Byte3| Bytes], Bytes) :-
		Code =< 0xFFFF,
		!,
		Byte1 is 0xE0 \/ (Code >> 12),
		Byte2 is 0x80 \/ ((Code >> 6) /\ 0x3F),
		Byte3 is 0x80 \/ (Code /\ 0x3F).
	code_to_bytes(Code, [Byte1, Byte2, Byte3, Byte4| Bytes], Bytes) :-
		Byte1 is 0xF0 \/ ((Code >> 18) /\ 0x07),
		Byte2 is 0x80 \/ ((Code >> 12) /\ 0x3F),
		Byte3 is 0x80 \/ ((Code >> 6) /\ 0x3F),
		Byte4 is 0x80 \/ (Code /\ 0x3F).

	bytes_to_codes_([], []).
	bytes_to_codes_([Byte| Bytes], [Byte| Codes]) :-
		Byte =< 0x7F,
		!,
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([Byte1, Byte2| Bytes], [Code| Codes]) :-
		Byte1 >= 0xC2,
		Byte1 =< 0xDF,
		^^continuation_byte(Byte2),
		!,
		Code is ((Byte1 /\ 0x1F) << 6) \/ (Byte2 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([0xE0, Byte2, Byte3| Bytes], [Code| Codes]) :-
		Byte2 >= 0xA0,
		Byte2 =< 0xBF,
		^^continuation_byte(Byte3),
		!,
		Code is ((0xE0 /\ 0x0F) << 12) \/ ((Byte2 /\ 0x3F) << 6) \/ (Byte3 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([Byte1, Byte2, Byte3| Bytes], [Code| Codes]) :-
		Byte1 >= 0xE1,
		Byte1 =< 0xEC,
		^^continuation_byte(Byte2),
		^^continuation_byte(Byte3),
		!,
		Code is ((Byte1 /\ 0x0F) << 12) \/ ((Byte2 /\ 0x3F) << 6) \/ (Byte3 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([0xED, Byte2, Byte3| Bytes], [Code| Codes]) :-
		Byte2 >= 0x80,
		Byte2 =< 0x9F,
		^^continuation_byte(Byte3),
		!,
		Code is ((0xED /\ 0x0F) << 12) \/ ((Byte2 /\ 0x3F) << 6) \/ (Byte3 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([Byte1, Byte2, Byte3| Bytes], [Code| Codes]) :-
		Byte1 >= 0xEE,
		Byte1 =< 0xEF,
		^^continuation_byte(Byte2),
		^^continuation_byte(Byte3),
		!,
		Code is ((Byte1 /\ 0x0F) << 12) \/ ((Byte2 /\ 0x3F) << 6) \/ (Byte3 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([0xF0, Byte2, Byte3, Byte4| Bytes], [Code| Codes]) :-
		Byte2 >= 0x90,
		Byte2 =< 0xBF,
		^^continuation_byte(Byte3),
		^^continuation_byte(Byte4),
		!,
		Code is ((0xF0 /\ 0x07) << 18) \/ ((Byte2 /\ 0x3F) << 12) \/ ((Byte3 /\ 0x3F) << 6) \/ (Byte4 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([Byte1, Byte2, Byte3, Byte4| Bytes], [Code| Codes]) :-
		Byte1 >= 0xF1,
		Byte1 =< 0xF3,
		^^continuation_byte(Byte2),
		^^continuation_byte(Byte3),
		^^continuation_byte(Byte4),
		!,
		Code is ((Byte1 /\ 0x07) << 18) \/ ((Byte2 /\ 0x3F) << 12) \/ ((Byte3 /\ 0x3F) << 6) \/ (Byte4 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).
	bytes_to_codes_([0xF4, Byte2, Byte3, Byte4| Bytes], [Code| Codes]) :-
		Byte2 >= 0x80,
		Byte2 =< 0x8F,
		^^continuation_byte(Byte3),
		^^continuation_byte(Byte4),
		!,
		Code is ((0xF4 /\ 0x07) << 18) \/ ((Byte2 /\ 0x3F) << 12) \/ ((Byte3 /\ 0x3F) << 6) \/ (Byte4 /\ 0x3F),
		bytes_to_codes_(Bytes, Codes).

:- end_object.


:- category(utf_16_character_set(_Endian_),
	extends(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-16 character set implementation parameterized by byte order.',
		parnames is ['Endian']
	]).

	codes_to_bytes(Codes, Bytes) :-
		codes_to_bytes(Codes, _Endian_, Bytes, []).

	bytes_to_codes(Bytes, Codes) :-
		bytes_to_codes(Bytes, _Endian_, Codes).

	codes_to_bytes([], _, Bytes, Bytes).
	codes_to_bytes([Code| Codes], Endian, Bytes0, Bytes) :-
		^^valid_unicode_scalar(Code),
		(   Code =< 0xFFFF ->
			^^word_bytes(Endian, Code, Byte1, Byte2),
			Bytes0 = [Byte1, Byte2| Bytes1]
		;   Pair is Code - 0x10000,
			High is 0xD800 + (Pair >> 10),
			Low is 0xDC00 + (Pair /\ 0x3FF),
			^^word_bytes(Endian, High, Byte1, Byte2),
			^^word_bytes(Endian, Low, Byte3, Byte4),
			Bytes0 = [Byte1, Byte2, Byte3, Byte4| Bytes1]
		),
		codes_to_bytes(Codes, Endian, Bytes1, Bytes).

	bytes_to_codes([], _, []).
	bytes_to_codes([Byte1, Byte2| Bytes], Endian, [Code| Codes]) :-
		^^bytes_word(Endian, Byte1, Byte2, Word),
		(   ^^high_surrogate(Word) ->
			Bytes = [Byte3, Byte4| Rest],
			^^bytes_word(Endian, Byte3, Byte4, Low),
			^^low_surrogate(Low),
			Code is 0x10000 + ((Word - 0xD800) << 10) + (Low - 0xDC00)
		;   ^^low_surrogate(Word) ->
			fail
		;   ^^valid_unicode_scalar(Word),
			Code = Word,
			Rest = Bytes
		),
		bytes_to_codes(Rest, Endian, Codes).

:- end_category.


:- category(utf_32_character_set(_Endian_),
	extends(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-32 character set implementation parameterized by byte order.',
		parnames is ['Endian']
	]).

	codes_to_bytes(Codes, Bytes) :-
		codes_to_bytes(Codes, _Endian_, Bytes, []).

	bytes_to_codes(Bytes, Codes) :-
		bytes_to_codes(Bytes, _Endian_, Codes).

	codes_to_bytes([], _, Bytes, Bytes).
	codes_to_bytes([Code| Codes], Endian, Bytes0, Bytes) :-
		^^valid_unicode_scalar(Code),
		^^dword_bytes(Endian, Code, Byte1, Byte2, Byte3, Byte4),
		Bytes0 = [Byte1, Byte2, Byte3, Byte4| Bytes1],
		codes_to_bytes(Codes, Endian, Bytes1, Bytes).

	bytes_to_codes([], _, []).
	bytes_to_codes([Byte1, Byte2, Byte3, Byte4| Bytes], Endian, [Code| Codes]) :-
		^^bytes_dword(Endian, Byte1, Byte2, Byte3, Byte4, Code),
		^^valid_unicode_scalar(Code),
		bytes_to_codes(Bytes, Endian, Codes).

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


:- object(iso_8859_1,
	imports(single_byte_character_set(0xFF))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-1 character set encoder and decoder.'
	]).

	preferred_mime_name('ISO-8859-1').

	name('ISO_8859-1:1987').

	alias('iso-ir-100').
	alias('ISO_8859-1').
	alias(latin1).
	alias(l1).
	alias('IBM819').
	alias('CP819').
	alias('csISOLatin1').

	mibenum(4).

:- end_object.


:- object(iso_8859_2,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-2 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x0104).
	mapping(0xA2, 0x02D8).
	mapping(0xA3, 0x0141).
	mapping(0xA5, 0x013D).
	mapping(0xA6, 0x015A).
	mapping(0xA9, 0x0160).
	mapping(0xAA, 0x015E).
	mapping(0xAB, 0x0164).
	mapping(0xAC, 0x0179).
	mapping(0xAE, 0x017D).
	mapping(0xAF, 0x017B).
	mapping(0xB1, 0x0105).
	mapping(0xB2, 0x02DB).
	mapping(0xB3, 0x0142).
	mapping(0xB5, 0x013E).
	mapping(0xB6, 0x015B).
	mapping(0xB7, 0x02C7).
	mapping(0xB9, 0x0161).
	mapping(0xBA, 0x015F).
	mapping(0xBB, 0x0165).
	mapping(0xBC, 0x017A).
	mapping(0xBD, 0x02DD).
	mapping(0xBE, 0x017E).
	mapping(0xBF, 0x017C).
	mapping(0xC0, 0x0154).
	mapping(0xC3, 0x0102).
	mapping(0xC5, 0x0139).
	mapping(0xC6, 0x0106).
	mapping(0xC8, 0x010C).
	mapping(0xCA, 0x0118).
	mapping(0xCC, 0x011A).
	mapping(0xCF, 0x010E).
	mapping(0xD0, 0x0110).
	mapping(0xD1, 0x0143).
	mapping(0xD2, 0x0147).
	mapping(0xD5, 0x0150).
	mapping(0xD8, 0x0158).
	mapping(0xD9, 0x016E).
	mapping(0xDB, 0x0170).
	mapping(0xDE, 0x0162).
	mapping(0xE0, 0x0155).
	mapping(0xE3, 0x0103).
	mapping(0xE5, 0x013A).
	mapping(0xE6, 0x0107).
	mapping(0xE8, 0x010D).
	mapping(0xEA, 0x0119).
	mapping(0xEC, 0x011B).
	mapping(0xEF, 0x010F).
	mapping(0xF0, 0x0111).
	mapping(0xF1, 0x0144).
	mapping(0xF2, 0x0148).
	mapping(0xF5, 0x0151).
	mapping(0xF8, 0x0159).
	mapping(0xF9, 0x016F).
	mapping(0xFB, 0x0171).
	mapping(0xFE, 0x0163).
	mapping(0xFF, 0x02D9).

	preferred_mime_name('ISO-8859-2').

	name('ISO_8859-2:1987').

	alias('iso-ir-101').
	alias('ISO_8859-2').
	alias('ISO-8859-2').
	alias(latin2).
	alias(l2).
	alias('csISOLatin2').

	mibenum(5).

:- end_object.


:- object(iso_8859_3,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-3 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x0126).
	mapping(0xA2, 0x02D8).
	mapping(0xA6, 0x0124).
	mapping(0xA9, 0x0130).
	mapping(0xAA, 0x015E).
	mapping(0xAB, 0x011E).
	mapping(0xAC, 0x0134).
	mapping(0xAF, 0x017B).
	mapping(0xB1, 0x0127).
	mapping(0xB6, 0x0125).
	mapping(0xB9, 0x0131).
	mapping(0xBA, 0x015F).
	mapping(0xBB, 0x011F).
	mapping(0xBC, 0x0135).
	mapping(0xBF, 0x017C).
	mapping(0xC5, 0x010A).
	mapping(0xC6, 0x0108).
	mapping(0xD5, 0x0120).
	mapping(0xD8, 0x011C).
	mapping(0xDD, 0x016C).
	mapping(0xDE, 0x015C).
	mapping(0xE5, 0x010B).
	mapping(0xE6, 0x0109).
	mapping(0xF5, 0x0121).
	mapping(0xF8, 0x011D).
	mapping(0xFD, 0x016D).
	mapping(0xFE, 0x015D).
	mapping(0xFF, 0x02D9).

	preferred_mime_name('ISO-8859-3').

	name('ISO_8859-3:1988').

	alias('iso-ir-109').
	alias('ISO_8859-3').
	alias('ISO-8859-3').
	alias(latin3).
	alias(l3).
	alias('csISOLatin3').

	mibenum(6).

:- end_object.


:- object(iso_8859_4,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-4 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x0104).
	mapping(0xA2, 0x0138).
	mapping(0xA3, 0x0156).
	mapping(0xA5, 0x0128).
	mapping(0xA6, 0x013B).
	mapping(0xA9, 0x0160).
	mapping(0xAA, 0x0112).
	mapping(0xAB, 0x0122).
	mapping(0xAC, 0x0166).
	mapping(0xAE, 0x017D).
	mapping(0xB1, 0x0105).
	mapping(0xB2, 0x02DB).
	mapping(0xB3, 0x0157).
	mapping(0xB5, 0x0129).
	mapping(0xB6, 0x013C).
	mapping(0xB7, 0x02C7).
	mapping(0xB9, 0x0161).
	mapping(0xBA, 0x0113).
	mapping(0xBB, 0x0123).
	mapping(0xBC, 0x0167).
	mapping(0xBD, 0x014A).
	mapping(0xBE, 0x017E).
	mapping(0xBF, 0x014B).
	mapping(0xC0, 0x0100).
	mapping(0xC7, 0x012E).
	mapping(0xC8, 0x010C).
	mapping(0xCA, 0x0118).
	mapping(0xCC, 0x0116).
	mapping(0xCF, 0x012A).
	mapping(0xD0, 0x0110).
	mapping(0xD1, 0x0145).
	mapping(0xD2, 0x014C).
	mapping(0xD3, 0x0136).
	mapping(0xD9, 0x0172).
	mapping(0xDD, 0x0168).
	mapping(0xDE, 0x016A).
	mapping(0xE0, 0x0101).
	mapping(0xE7, 0x012F).
	mapping(0xE8, 0x010D).
	mapping(0xEA, 0x0119).
	mapping(0xEC, 0x0117).
	mapping(0xEF, 0x012B).
	mapping(0xF0, 0x0111).
	mapping(0xF1, 0x0146).
	mapping(0xF2, 0x014D).
	mapping(0xF3, 0x0137).
	mapping(0xF9, 0x0173).
	mapping(0xFD, 0x0169).
	mapping(0xFE, 0x016B).
	mapping(0xFF, 0x02D9).

	preferred_mime_name('ISO-8859-4').

	name('ISO_8859-4:1988').

	alias('iso-ir-110').
	alias('ISO_8859-4').
	alias('ISO-8859-4').
	alias(latin4).
	alias(l4).
	alias('csISOLatin4').

	mibenum(7).

:- end_object.


:- object(iso_8859_9,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-9 character set encoder and decoder.'
	]).

	mapping(0xD0, 0x011E).
	mapping(0xDD, 0x0130).
	mapping(0xDE, 0x015E).
	mapping(0xF0, 0x011F).
	mapping(0xFD, 0x0131).
	mapping(0xFE, 0x015F).

	preferred_mime_name('ISO-8859-9').

	name('ISO_8859-9:1989').

	alias('iso-ir-148').
	alias('ISO_8859-9').
	alias('ISO-8859-9').
	alias(latin5).
	alias(l5).
	alias('csISOLatin5').

	mibenum(12).

:- end_object.


:- object(iso_8859_10,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-10 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x0104).
	mapping(0xA2, 0x0112).
	mapping(0xA3, 0x0122).
	mapping(0xA4, 0x012A).
	mapping(0xA5, 0x0128).
	mapping(0xA6, 0x0136).
	mapping(0xA8, 0x013B).
	mapping(0xA9, 0x0110).
	mapping(0xAA, 0x0160).
	mapping(0xAB, 0x0166).
	mapping(0xAC, 0x017D).
	mapping(0xAE, 0x016A).
	mapping(0xAF, 0x014A).
	mapping(0xB1, 0x0105).
	mapping(0xB2, 0x0113).
	mapping(0xB3, 0x0123).
	mapping(0xB4, 0x012B).
	mapping(0xB5, 0x0129).
	mapping(0xB6, 0x0137).
	mapping(0xB8, 0x013C).
	mapping(0xB9, 0x0111).
	mapping(0xBA, 0x0161).
	mapping(0xBB, 0x0167).
	mapping(0xBC, 0x017E).
	mapping(0xBD, 0x2015).
	mapping(0xBE, 0x016B).
	mapping(0xBF, 0x014B).
	mapping(0xC0, 0x0100).
	mapping(0xC7, 0x012E).
	mapping(0xC8, 0x010C).
	mapping(0xCA, 0x0118).
	mapping(0xCC, 0x0116).
	mapping(0xD1, 0x0145).
	mapping(0xD2, 0x014C).
	mapping(0xD7, 0x0168).
	mapping(0xD9, 0x0172).
	mapping(0xE0, 0x0101).
	mapping(0xE7, 0x012F).
	mapping(0xE8, 0x010D).
	mapping(0xEA, 0x0119).
	mapping(0xEC, 0x0117).
	mapping(0xF1, 0x0146).
	mapping(0xF2, 0x014D).
	mapping(0xF7, 0x0169).
	mapping(0xF9, 0x0173).
	mapping(0xFF, 0x0138).

	preferred_mime_name('ISO-8859-10').

	name('ISO-8859-10').

	alias('iso-ir-157').
	alias(l6).
	alias('ISO_8859-10:1992').
	alias('csISOLatin6').
	alias(latin6).

	mibenum(13).

:- end_object.


:- object(iso_8859_13,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-13 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x201D).
	mapping(0xA5, 0x201E).
	mapping(0xA8, 0x00D8).
	mapping(0xAA, 0x0156).
	mapping(0xAF, 0x00C6).
	mapping(0xB4, 0x201C).
	mapping(0xB8, 0x00F8).
	mapping(0xBA, 0x0157).
	mapping(0xBF, 0x00E6).
	mapping(0xC0, 0x0104).
	mapping(0xC1, 0x012E).
	mapping(0xC2, 0x0100).
	mapping(0xC3, 0x0106).
	mapping(0xC6, 0x0118).
	mapping(0xC7, 0x0112).
	mapping(0xC8, 0x010C).
	mapping(0xCA, 0x0179).
	mapping(0xCB, 0x0116).
	mapping(0xCC, 0x0122).
	mapping(0xCD, 0x0136).
	mapping(0xCE, 0x012A).
	mapping(0xCF, 0x013B).
	mapping(0xD0, 0x0160).
	mapping(0xD1, 0x0143).
	mapping(0xD2, 0x0145).
	mapping(0xD4, 0x014C).
	mapping(0xD8, 0x0172).
	mapping(0xD9, 0x0141).
	mapping(0xDA, 0x015A).
	mapping(0xDB, 0x016A).
	mapping(0xDD, 0x017B).
	mapping(0xDE, 0x017D).
	mapping(0xE0, 0x0105).
	mapping(0xE1, 0x012F).
	mapping(0xE2, 0x0101).
	mapping(0xE3, 0x0107).
	mapping(0xE6, 0x0119).
	mapping(0xE7, 0x0113).
	mapping(0xE8, 0x010D).
	mapping(0xEA, 0x017A).
	mapping(0xEB, 0x0117).
	mapping(0xEC, 0x0123).
	mapping(0xED, 0x0137).
	mapping(0xEE, 0x012B).
	mapping(0xEF, 0x013C).
	mapping(0xF0, 0x0161).
	mapping(0xF1, 0x0144).
	mapping(0xF2, 0x0146).
	mapping(0xF4, 0x014D).
	mapping(0xF8, 0x0173).
	mapping(0xF9, 0x0142).
	mapping(0xFA, 0x015B).
	mapping(0xFB, 0x016B).
	mapping(0xFD, 0x017C).
	mapping(0xFE, 0x017E).
	mapping(0xFF, 0x2019).

	preferred_mime_name('ISO-8859-13').

	name('ISO-8859-13').

	alias('csISO885913').

	mibenum(109).

:- end_object.


:- object(iso_8859_14,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-14 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x1E02).
	mapping(0xA2, 0x1E03).
	mapping(0xA4, 0x010A).
	mapping(0xA5, 0x010B).
	mapping(0xA6, 0x1E0A).
	mapping(0xA8, 0x1E80).
	mapping(0xAA, 0x1E82).
	mapping(0xAB, 0x1E0B).
	mapping(0xAC, 0x1EF2).
	mapping(0xAF, 0x0178).
	mapping(0xB0, 0x1E1E).
	mapping(0xB1, 0x1E1F).
	mapping(0xB2, 0x0120).
	mapping(0xB3, 0x0121).
	mapping(0xB4, 0x1E40).
	mapping(0xB5, 0x1E41).
	mapping(0xB7, 0x1E56).
	mapping(0xB8, 0x1E81).
	mapping(0xB9, 0x1E57).
	mapping(0xBA, 0x1E83).
	mapping(0xBB, 0x1E60).
	mapping(0xBC, 0x1EF3).
	mapping(0xBD, 0x1E84).
	mapping(0xBE, 0x1E85).
	mapping(0xBF, 0x1E61).
	mapping(0xD0, 0x0174).
	mapping(0xD7, 0x1E6A).
	mapping(0xDE, 0x0176).
	mapping(0xF0, 0x0175).
	mapping(0xF7, 0x1E6B).
	mapping(0xFE, 0x0177).

	preferred_mime_name('ISO-8859-14').

	name('ISO-8859-14').

	alias('iso-ir-199').
	alias('ISO_8859-14:1998').
	alias('ISO_8859-14').
	alias(latin8).
	alias('iso-celtic').
	alias(l8).
	alias('csISO885914').

	mibenum(110).

:- end_object.


:- object(iso_8859_15,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-15 character set encoder and decoder.'
	]).

	mapping(0xA4, 0x20AC).
	mapping(0xA6, 0x0160).
	mapping(0xA8, 0x0161).
	mapping(0xB4, 0x017D).
	mapping(0xB8, 0x017E).
	mapping(0xBC, 0x0152).
	mapping(0xBD, 0x0153).
	mapping(0xBE, 0x0178).

	preferred_mime_name('ISO-8859-15').

	name('ISO-8859-15').

	alias('ISO_8859-15').
	alias('Latin-9').
	alias('csISO885915').

	mibenum(111).

:- end_object.


:- object(iso_8859_16,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'ISO-8859-16 character set encoder and decoder.'
	]).

	mapping(0xA1, 0x0104).
	mapping(0xA2, 0x0105).
	mapping(0xA3, 0x0141).
	mapping(0xA4, 0x20AC).
	mapping(0xA5, 0x201E).
	mapping(0xA6, 0x0160).
	mapping(0xA8, 0x0161).
	mapping(0xAA, 0x0218).
	mapping(0xAC, 0x0179).
	mapping(0xAE, 0x017A).
	mapping(0xAF, 0x017B).
	mapping(0xB2, 0x010C).
	mapping(0xB3, 0x0142).
	mapping(0xB4, 0x017D).
	mapping(0xB5, 0x201D).
	mapping(0xB8, 0x017E).
	mapping(0xB9, 0x010D).
	mapping(0xBA, 0x0219).
	mapping(0xBC, 0x0152).
	mapping(0xBD, 0x0153).
	mapping(0xBE, 0x0178).
	mapping(0xBF, 0x017C).
	mapping(0xC3, 0x0102).
	mapping(0xC5, 0x0106).
	mapping(0xD0, 0x0110).
	mapping(0xD1, 0x0143).
	mapping(0xD5, 0x0150).
	mapping(0xD7, 0x015A).
	mapping(0xD8, 0x0170).
	mapping(0xDD, 0x0118).
	mapping(0xDE, 0x021A).
	mapping(0xE3, 0x0103).
	mapping(0xE5, 0x0107).
	mapping(0xF0, 0x0111).
	mapping(0xF1, 0x0144).
	mapping(0xF5, 0x0151).
	mapping(0xF7, 0x015B).
	mapping(0xF8, 0x0171).
	mapping(0xFD, 0x0119).
	mapping(0xFE, 0x021B).

	preferred_mime_name('ISO-8859-16').

	name('ISO-8859-16').

	alias('iso-ir-226').
	alias('ISO_8859-16:2001').
	alias('ISO_8859-16').
	alias(latin10).
	alias(l10).
	alias('csISO885916').

	mibenum(112).

:- end_object.


:- object(windows_1250,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Windows-1250 character set encoder and decoder.'
	]).

	mapping(0x80, 0x20AC).
	mapping(0x82, 0x201A).
	mapping(0x84, 0x201E).
	mapping(0x85, 0x2026).
	mapping(0x86, 0x2020).
	mapping(0x87, 0x2021).
	mapping(0x89, 0x2030).
	mapping(0x8A, 0x0160).
	mapping(0x8B, 0x2039).
	mapping(0x8C, 0x015A).
	mapping(0x8D, 0x0164).
	mapping(0x8E, 0x017D).
	mapping(0x8F, 0x0179).
	mapping(0x91, 0x2018).
	mapping(0x92, 0x2019).
	mapping(0x93, 0x201C).
	mapping(0x94, 0x201D).
	mapping(0x95, 0x2022).
	mapping(0x96, 0x2013).
	mapping(0x97, 0x2014).
	mapping(0x99, 0x2122).
	mapping(0x9A, 0x0161).
	mapping(0x9B, 0x203A).
	mapping(0x9C, 0x015B).
	mapping(0x9D, 0x0165).
	mapping(0x9E, 0x017E).
	mapping(0x9F, 0x017A).
	mapping(0xA1, 0x02C7).
	mapping(0xA2, 0x02D8).
	mapping(0xA3, 0x0141).
	mapping(0xA5, 0x0104).
	mapping(0xAA, 0x015E).
	mapping(0xAF, 0x017B).
	mapping(0xB2, 0x02DB).
	mapping(0xB3, 0x0142).
	mapping(0xB9, 0x0105).
	mapping(0xBA, 0x015F).
	mapping(0xBC, 0x013D).
	mapping(0xBD, 0x02DD).
	mapping(0xBE, 0x013E).
	mapping(0xBF, 0x017C).
	mapping(0xC0, 0x0154).
	mapping(0xC3, 0x0102).
	mapping(0xC5, 0x0139).
	mapping(0xC6, 0x0106).
	mapping(0xC8, 0x010C).
	mapping(0xCA, 0x0118).
	mapping(0xCC, 0x011A).
	mapping(0xCF, 0x010E).
	mapping(0xD0, 0x0110).
	mapping(0xD1, 0x0143).
	mapping(0xD2, 0x0147).
	mapping(0xD5, 0x0150).
	mapping(0xD8, 0x0158).
	mapping(0xD9, 0x016E).
	mapping(0xDB, 0x0170).
	mapping(0xDE, 0x0162).
	mapping(0xE0, 0x0155).
	mapping(0xE3, 0x0103).
	mapping(0xE5, 0x013A).
	mapping(0xE6, 0x0107).
	mapping(0xE8, 0x010D).
	mapping(0xEA, 0x0119).
	mapping(0xEC, 0x011B).
	mapping(0xEF, 0x010F).
	mapping(0xF0, 0x0111).
	mapping(0xF1, 0x0144).
	mapping(0xF2, 0x0148).
	mapping(0xF5, 0x0151).
	mapping(0xF8, 0x0159).
	mapping(0xF9, 0x016F).
	mapping(0xFB, 0x0171).
	mapping(0xFE, 0x0163).
	mapping(0xFF, 0x02D9).

	undefined(0x81, 0x81).
	undefined(0x83, 0x83).
	undefined(0x88, 0x88).
	undefined(0x90, 0x90).
	undefined(0x98, 0x98).

	preferred_mime_name('windows-1250').

	name('windows-1250').

	alias('cswindows1250').

	mibenum(2250).

:- end_object.


:- object(windows_1251,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Windows-1251 character set encoder and decoder.'
	]).

	mapping(0x80, 0x0402).
	mapping(0x81, 0x0403).
	mapping(0x82, 0x201A).
	mapping(0x83, 0x0453).
	mapping(0x84, 0x201E).
	mapping(0x85, 0x2026).
	mapping(0x86, 0x2020).
	mapping(0x87, 0x2021).
	mapping(0x88, 0x20AC).
	mapping(0x89, 0x2030).
	mapping(0x8A, 0x0409).
	mapping(0x8B, 0x2039).
	mapping(0x8C, 0x040A).
	mapping(0x8D, 0x040C).
	mapping(0x8E, 0x040B).
	mapping(0x8F, 0x040F).
	mapping(0x90, 0x0452).
	mapping(0x91, 0x2018).
	mapping(0x92, 0x2019).
	mapping(0x93, 0x201C).
	mapping(0x94, 0x201D).
	mapping(0x95, 0x2022).
	mapping(0x96, 0x2013).
	mapping(0x97, 0x2014).
	mapping(0x99, 0x2122).
	mapping(0x9A, 0x0459).
	mapping(0x9B, 0x203A).
	mapping(0x9C, 0x045A).
	mapping(0x9D, 0x045C).
	mapping(0x9E, 0x045B).
	mapping(0x9F, 0x045F).
	mapping(0xA1, 0x040E).
	mapping(0xA2, 0x045E).
	mapping(0xA3, 0x0408).
	mapping(0xA5, 0x0490).
	mapping(0xA8, 0x0401).
	mapping(0xAA, 0x0404).
	mapping(0xAF, 0x0407).
	mapping(0xB2, 0x0406).
	mapping(0xB3, 0x0456).
	mapping(0xB4, 0x0491).
	mapping(0xB8, 0x0451).
	mapping(0xB9, 0x2116).
	mapping(0xBA, 0x0454).
	mapping(0xBC, 0x0458).
	mapping(0xBD, 0x0405).
	mapping(0xBE, 0x0455).
	mapping(0xBF, 0x0457).
	mapping(0xC0, 0x0410).
	mapping(0xC1, 0x0411).
	mapping(0xC2, 0x0412).
	mapping(0xC3, 0x0413).
	mapping(0xC4, 0x0414).
	mapping(0xC5, 0x0415).
	mapping(0xC6, 0x0416).
	mapping(0xC7, 0x0417).
	mapping(0xC8, 0x0418).
	mapping(0xC9, 0x0419).
	mapping(0xCA, 0x041A).
	mapping(0xCB, 0x041B).
	mapping(0xCC, 0x041C).
	mapping(0xCD, 0x041D).
	mapping(0xCE, 0x041E).
	mapping(0xCF, 0x041F).
	mapping(0xD0, 0x0420).
	mapping(0xD1, 0x0421).
	mapping(0xD2, 0x0422).
	mapping(0xD3, 0x0423).
	mapping(0xD4, 0x0424).
	mapping(0xD5, 0x0425).
	mapping(0xD6, 0x0426).
	mapping(0xD7, 0x0427).
	mapping(0xD8, 0x0428).
	mapping(0xD9, 0x0429).
	mapping(0xDA, 0x042A).
	mapping(0xDB, 0x042B).
	mapping(0xDC, 0x042C).
	mapping(0xDD, 0x042D).
	mapping(0xDE, 0x042E).
	mapping(0xDF, 0x042F).
	mapping(0xE0, 0x0430).
	mapping(0xE1, 0x0431).
	mapping(0xE2, 0x0432).
	mapping(0xE3, 0x0433).
	mapping(0xE4, 0x0434).
	mapping(0xE5, 0x0435).
	mapping(0xE6, 0x0436).
	mapping(0xE7, 0x0437).
	mapping(0xE8, 0x0438).
	mapping(0xE9, 0x0439).
	mapping(0xEA, 0x043A).
	mapping(0xEB, 0x043B).
	mapping(0xEC, 0x043C).
	mapping(0xED, 0x043D).
	mapping(0xEE, 0x043E).
	mapping(0xEF, 0x043F).
	mapping(0xF0, 0x0440).
	mapping(0xF1, 0x0441).
	mapping(0xF2, 0x0442).
	mapping(0xF3, 0x0443).
	mapping(0xF4, 0x0444).
	mapping(0xF5, 0x0445).
	mapping(0xF6, 0x0446).
	mapping(0xF7, 0x0447).
	mapping(0xF8, 0x0448).
	mapping(0xF9, 0x0449).
	mapping(0xFA, 0x044A).
	mapping(0xFB, 0x044B).
	mapping(0xFC, 0x044C).
	mapping(0xFD, 0x044D).
	mapping(0xFE, 0x044E).
	mapping(0xFF, 0x044F).

	undefined(0x98, 0x98).

	preferred_mime_name('windows-1251').

	name('windows-1251').

	alias('cswindows1251').

	mibenum(2251).

:- end_object.


:- object(windows_1253,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Windows-1253 character set encoder and decoder.'
	]).

	mapping(0x80, 0x20AC).
	mapping(0x82, 0x201A).
	mapping(0x83, 0x0192).
	mapping(0x84, 0x201E).
	mapping(0x85, 0x2026).
	mapping(0x86, 0x2020).
	mapping(0x87, 0x2021).
	mapping(0x89, 0x2030).
	mapping(0x8B, 0x2039).
	mapping(0x91, 0x2018).
	mapping(0x92, 0x2019).
	mapping(0x93, 0x201C).
	mapping(0x94, 0x201D).
	mapping(0x95, 0x2022).
	mapping(0x96, 0x2013).
	mapping(0x97, 0x2014).
	mapping(0x99, 0x2122).
	mapping(0x9B, 0x203A).
	mapping(0xA1, 0x0385).
	mapping(0xA2, 0x0386).
	mapping(0xAF, 0x2015).
	mapping(0xB4, 0x0384).
	mapping(0xB8, 0x0388).
	mapping(0xB9, 0x0389).
	mapping(0xBA, 0x038A).
	mapping(0xBC, 0x038C).
	mapping(0xBE, 0x038E).
	mapping(0xBF, 0x038F).
	mapping(0xC0, 0x0390).
	mapping(0xC1, 0x0391).
	mapping(0xC2, 0x0392).
	mapping(0xC3, 0x0393).
	mapping(0xC4, 0x0394).
	mapping(0xC5, 0x0395).
	mapping(0xC6, 0x0396).
	mapping(0xC7, 0x0397).
	mapping(0xC8, 0x0398).
	mapping(0xC9, 0x0399).
	mapping(0xCA, 0x039A).
	mapping(0xCB, 0x039B).
	mapping(0xCC, 0x039C).
	mapping(0xCD, 0x039D).
	mapping(0xCE, 0x039E).
	mapping(0xCF, 0x039F).
	mapping(0xD0, 0x03A0).
	mapping(0xD1, 0x03A1).
	mapping(0xD3, 0x03A3).
	mapping(0xD4, 0x03A4).
	mapping(0xD5, 0x03A5).
	mapping(0xD6, 0x03A6).
	mapping(0xD7, 0x03A7).
	mapping(0xD8, 0x03A8).
	mapping(0xD9, 0x03A9).
	mapping(0xDA, 0x03AA).
	mapping(0xDB, 0x03AB).
	mapping(0xDC, 0x03AC).
	mapping(0xDD, 0x03AD).
	mapping(0xDE, 0x03AE).
	mapping(0xDF, 0x03AF).
	mapping(0xE0, 0x03B0).
	mapping(0xE1, 0x03B1).
	mapping(0xE2, 0x03B2).
	mapping(0xE3, 0x03B3).
	mapping(0xE4, 0x03B4).
	mapping(0xE5, 0x03B5).
	mapping(0xE6, 0x03B6).
	mapping(0xE7, 0x03B7).
	mapping(0xE8, 0x03B8).
	mapping(0xE9, 0x03B9).
	mapping(0xEA, 0x03BA).
	mapping(0xEB, 0x03BB).
	mapping(0xEC, 0x03BC).
	mapping(0xED, 0x03BD).
	mapping(0xEE, 0x03BE).
	mapping(0xEF, 0x03BF).
	mapping(0xF0, 0x03C0).
	mapping(0xF1, 0x03C1).
	mapping(0xF2, 0x03C2).
	mapping(0xF3, 0x03C3).
	mapping(0xF4, 0x03C4).
	mapping(0xF5, 0x03C5).
	mapping(0xF6, 0x03C6).
	mapping(0xF7, 0x03C7).
	mapping(0xF8, 0x03C8).
	mapping(0xF9, 0x03C9).
	mapping(0xFA, 0x03CA).
	mapping(0xFB, 0x03CB).
	mapping(0xFC, 0x03CC).
	mapping(0xFD, 0x03CD).
	mapping(0xFE, 0x03CE).

	undefined(0x81, 0x81).
	undefined(0x88, 0x88).
	undefined(0x8A, 0x8A).
	undefined(0x8C, 0x8C).
	undefined(0x8D, 0x8D).
	undefined(0x8E, 0x8E).
	undefined(0x8F, 0x8F).
	undefined(0x90, 0x90).
	undefined(0x98, 0x98).
	undefined(0x9A, 0x9A).
	undefined(0x9C, 0x9C).
	undefined(0x9D, 0x9D).
	undefined(0x9E, 0x9E).
	undefined(0x9F, 0x9F).
	undefined(0xAA, 0xAA).
	undefined(0xD2, 0xD2).
	undefined(0xFF, 0xFF).

	preferred_mime_name('windows-1253').

	name('windows-1253').

	alias('cswindows1253').

	mibenum(2253).

:- end_object.


:- object(windows_1254,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Windows-1254 character set encoder and decoder.'
	]).

	mapping(0x80, 0x20AC).
	mapping(0x82, 0x201A).
	mapping(0x83, 0x0192).
	mapping(0x84, 0x201E).
	mapping(0x85, 0x2026).
	mapping(0x86, 0x2020).
	mapping(0x87, 0x2021).
	mapping(0x88, 0x02C6).
	mapping(0x89, 0x2030).
	mapping(0x8A, 0x0160).
	mapping(0x8B, 0x2039).
	mapping(0x8C, 0x0152).
	mapping(0x91, 0x2018).
	mapping(0x92, 0x2019).
	mapping(0x93, 0x201C).
	mapping(0x94, 0x201D).
	mapping(0x95, 0x2022).
	mapping(0x96, 0x2013).
	mapping(0x97, 0x2014).
	mapping(0x98, 0x02DC).
	mapping(0x99, 0x2122).
	mapping(0x9A, 0x0161).
	mapping(0x9B, 0x203A).
	mapping(0x9C, 0x0153).
	mapping(0x9F, 0x0178).
	mapping(0xD0, 0x011E).
	mapping(0xDD, 0x0130).
	mapping(0xDE, 0x015E).
	mapping(0xF0, 0x011F).
	mapping(0xFD, 0x0131).
	mapping(0xFE, 0x015F).

	undefined(0x81, 0x81).
	undefined(0x8D, 0x8D).
	undefined(0x8E, 0x8E).
	undefined(0x8F, 0x8F).
	undefined(0x90, 0x90).
	undefined(0x9D, 0x9D).
	undefined(0x9E, 0x9E).

	preferred_mime_name('windows-1254').

	name('windows-1254').

	alias('cswindows1254').

	mibenum(2254).

:- end_object.


:- object(windows_1257,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Windows-1257 character set encoder and decoder.'
	]).

	mapping(0x80, 0x20AC).
	mapping(0x82, 0x201A).
	mapping(0x84, 0x201E).
	mapping(0x85, 0x2026).
	mapping(0x86, 0x2020).
	mapping(0x87, 0x2021).
	mapping(0x89, 0x2030).
	mapping(0x8B, 0x2039).
	mapping(0x8D, 0x00A8).
	mapping(0x8E, 0x02C7).
	mapping(0x8F, 0x00B8).
	mapping(0x91, 0x2018).
	mapping(0x92, 0x2019).
	mapping(0x93, 0x201C).
	mapping(0x94, 0x201D).
	mapping(0x95, 0x2022).
	mapping(0x96, 0x2013).
	mapping(0x97, 0x2014).
	mapping(0x99, 0x2122).
	mapping(0x9B, 0x203A).
	mapping(0x9D, 0x00AF).
	mapping(0x9E, 0x02DB).
	mapping(0xA8, 0x00D8).
	mapping(0xAA, 0x0156).
	mapping(0xAF, 0x00C6).
	mapping(0xB8, 0x00F8).
	mapping(0xBA, 0x0157).
	mapping(0xBF, 0x00E6).
	mapping(0xC0, 0x0104).
	mapping(0xC1, 0x012E).
	mapping(0xC2, 0x0100).
	mapping(0xC3, 0x0106).
	mapping(0xC6, 0x0118).
	mapping(0xC7, 0x0112).
	mapping(0xC8, 0x010C).
	mapping(0xCA, 0x0179).
	mapping(0xCB, 0x0116).
	mapping(0xCC, 0x0122).
	mapping(0xCD, 0x0136).
	mapping(0xCE, 0x012A).
	mapping(0xCF, 0x013B).
	mapping(0xD0, 0x0160).
	mapping(0xD1, 0x0143).
	mapping(0xD2, 0x0145).
	mapping(0xD4, 0x014C).
	mapping(0xD8, 0x0172).
	mapping(0xD9, 0x0141).
	mapping(0xDA, 0x015A).
	mapping(0xDB, 0x016A).
	mapping(0xDD, 0x017B).
	mapping(0xDE, 0x017D).
	mapping(0xE0, 0x0105).
	mapping(0xE1, 0x012F).
	mapping(0xE2, 0x0101).
	mapping(0xE3, 0x0107).
	mapping(0xE6, 0x0119).
	mapping(0xE7, 0x0113).
	mapping(0xE8, 0x010D).
	mapping(0xEA, 0x017A).
	mapping(0xEB, 0x0117).
	mapping(0xEC, 0x0123).
	mapping(0xED, 0x0137).
	mapping(0xEE, 0x012B).
	mapping(0xEF, 0x013C).
	mapping(0xF0, 0x0161).
	mapping(0xF1, 0x0144).
	mapping(0xF2, 0x0146).
	mapping(0xF4, 0x014D).
	mapping(0xF8, 0x0173).
	mapping(0xF9, 0x0142).
	mapping(0xFA, 0x015B).
	mapping(0xFB, 0x016B).
	mapping(0xFD, 0x017C).
	mapping(0xFE, 0x017E).
	mapping(0xFF, 0x02D9).

	undefined(0x81, 0x81).
	undefined(0x83, 0x83).
	undefined(0x88, 0x88).
	undefined(0x8A, 0x8A).
	undefined(0x8C, 0x8C).
	undefined(0x90, 0x90).
	undefined(0x98, 0x98).
	undefined(0x9A, 0x9A).
	undefined(0x9C, 0x9C).
	undefined(0x9F, 0x9F).
	undefined(0xA1, 0xA1).
	undefined(0xA5, 0xA5).

	preferred_mime_name('windows-1257').

	name('windows-1257').

	alias('cswindows1257').

	mibenum(2257).

:- end_object.


:- object(windows_1252,
	imports(mapped_single_byte_character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Windows-1252 character set encoder and decoder.'
	]).

	mapping(0x80, 0x20AC).
	mapping(0x82, 0x201A).
	mapping(0x83, 0x0192).
	mapping(0x84, 0x201E).
	mapping(0x85, 0x2026).
	mapping(0x86, 0x2020).
	mapping(0x87, 0x2021).
	mapping(0x88, 0x02C6).
	mapping(0x89, 0x2030).
	mapping(0x8A, 0x0160).
	mapping(0x8B, 0x2039).
	mapping(0x8C, 0x0152).
	mapping(0x8E, 0x017D).
	mapping(0x91, 0x2018).
	mapping(0x92, 0x2019).
	mapping(0x93, 0x201C).
	mapping(0x94, 0x201D).
	mapping(0x95, 0x2022).
	mapping(0x96, 0x2013).
	mapping(0x97, 0x2014).
	mapping(0x98, 0x02DC).
	mapping(0x99, 0x2122).
	mapping(0x9A, 0x0161).
	mapping(0x9B, 0x203A).
	mapping(0x9C, 0x0153).
	mapping(0x9E, 0x017E).
	mapping(0x9F, 0x0178).

	undefined(0x81, 0x81).
	undefined(0x8D, 0x8D).
	undefined(0x8F, 0x8F).
	undefined(0x90, 0x90).
	undefined(0x9D, 0x9D).

	preferred_mime_name('windows-1252').

	name('windows-1252').

	alias('cswindows1252').

	mibenum(2252).

:- end_object.


:- object(utf_8,
	extends(utf_8_character_set),
	imports(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-8 character set encoder and decoder.'
	]).

	preferred_mime_name('UTF-8').

	name('UTF-8').

	alias('csUTF8').

	mibenum(106).

:- end_object.


:- object(utf_16le,
	imports(utf_16_character_set(little_endian))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-16 little-endian character set encoder and decoder.'
	]).

	preferred_mime_name('UTF-16LE').

	name('UTF-16LE').

	alias('csUTF16LE').

	mibenum(1014).

:- end_object.


:- object(utf_16be,
	imports(utf_16_character_set(big_endian))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-16 big-endian character set encoder and decoder.'
	]).

	preferred_mime_name('UTF-16BE').

	name('UTF-16BE').

	alias('csUTF16BE').

	mibenum(1013).

:- end_object.


:- object(utf_32le,
	imports(utf_32_character_set(little_endian))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-32 little-endian character set encoder and decoder.'
	]).

	preferred_mime_name('UTF-32LE').

	name('UTF-32LE').

	alias('csUTF32LE').

	mibenum(1019).

:- end_object.


:- object(utf_32be,
	imports(utf_32_character_set(big_endian))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-32 big-endian character set encoder and decoder.'
	]).

	preferred_mime_name('UTF-32BE').

	name('UTF-32BE').

	alias('csUTF32BE').

	mibenum(1018).

:- end_object.
