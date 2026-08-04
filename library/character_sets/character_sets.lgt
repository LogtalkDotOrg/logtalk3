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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
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

	:- protected(endian_order/2).
	:- mode(endian_order(+atom, -atom), one).
	:- info(endian_order/2, [
		comment is 'Maps a character set endian parameter to a byte order.',
		argnames is ['Endian', 'Order']
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

	endian_order(little_endian, little).
	endian_order(big_endian, big).

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
