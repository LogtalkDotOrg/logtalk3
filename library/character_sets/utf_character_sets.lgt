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


:- object(utf_8_character_set,
	imports(character_set)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'UTF-8 character set implementation.'
	]).

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
		(	Code =< 0xFFFF ->
			^^word_bytes(Endian, Code, Byte1, Byte2),
			Bytes0 = [Byte1, Byte2| Bytes1]
		;	Pair is Code - 0x10000,
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
		(	^^high_surrogate(Word) ->
			Bytes = [Byte3, Byte4| Rest],
			^^bytes_word(Endian, Byte3, Byte4, Low),
			^^low_surrogate(Low),
			Code is 0x10000 + ((Word - 0xD800) << 10) + (Low - 0xDC00)
		;	^^low_surrogate(Word) ->
			fail
		;	^^valid_unicode_scalar(Word),
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
