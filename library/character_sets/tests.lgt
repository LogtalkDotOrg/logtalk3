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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Unit tests for the "character_sets" library.'
	]).

	cover(character_set).
	cover(single_byte_character_set(_)).
	cover(mapped_single_byte_character_set).
	cover(us_ascii).
	cover(iso_8859_1).
	cover(iso_8859_2).
	cover(iso_8859_3).
	cover(iso_8859_4).
	cover(iso_8859_9).
	cover(iso_8859_10).
	cover(iso_8859_13).
	cover(iso_8859_14).
	cover(iso_8859_15).
	cover(iso_8859_16).
	cover(windows_1250).
	cover(windows_1251).
	cover(windows_1253).
	cover(windows_1254).
	cover(windows_1257).
	cover(windows_1252).
	cover(utf_8).
	cover(utf_16_character_set(_)).
	cover(utf_16le).
	cover(utf_16be).
	cover(utf_32_character_set(_)).
	cover(utf_32le).
	cover(utf_32be).

	test(us_ascii_metadata_01, true((Preferred == 'US-ASCII', Name == 'US-ASCII', MIBenum == 3))) :-
		us_ascii::preferred_mime_name(Preferred),
		us_ascii::name(Name),
		us_ascii::mibenum(MIBenum).

	test(us_ascii_metadata_02, true) :-
		us_ascii::alias('iso-ir-6'),
		us_ascii::alias('ANSI_X3.4-1968'),
		us_ascii::alias('csASCII').

	test(iso_8859_1_metadata_01, true((Preferred == 'ISO-8859-1', Name == 'ISO_8859-1:1987', MIBenum == 4))) :-
		iso_8859_1::preferred_mime_name(Preferred),
		iso_8859_1::name(Name),
		iso_8859_1::mibenum(MIBenum).

	test(iso_8859_1_metadata_02, true) :-
		iso_8859_1::alias(latin1),
		iso_8859_1::alias('ISO_8859-1'),
		iso_8859_1::alias('csISOLatin1').

	test(iso_8859_2_metadata_01, true((Preferred == 'ISO-8859-2', Name == 'ISO_8859-2:1987', MIBenum == 5))) :-
		iso_8859_2::preferred_mime_name(Preferred),
		iso_8859_2::name(Name),
		iso_8859_2::mibenum(MIBenum).

	test(iso_8859_2_metadata_02, true) :-
		iso_8859_2::alias(latin2),
		iso_8859_2::alias('ISO_8859-2'),
		iso_8859_2::alias('csISOLatin2').

	test(iso_8859_3_metadata_01, true((Preferred == 'ISO-8859-3', Name == 'ISO_8859-3:1988', MIBenum == 6))) :-
		iso_8859_3::preferred_mime_name(Preferred),
		iso_8859_3::name(Name),
		iso_8859_3::mibenum(MIBenum).

	test(iso_8859_4_metadata_01, true((Preferred == 'ISO-8859-4', Name == 'ISO_8859-4:1988', MIBenum == 7))) :-
		iso_8859_4::preferred_mime_name(Preferred),
		iso_8859_4::name(Name),
		iso_8859_4::mibenum(MIBenum).

	test(iso_8859_9_metadata_01, true((Preferred == 'ISO-8859-9', Name == 'ISO_8859-9:1989', MIBenum == 12))) :-
		iso_8859_9::preferred_mime_name(Preferred),
		iso_8859_9::name(Name),
		iso_8859_9::mibenum(MIBenum).

	test(iso_8859_10_metadata_01, true((Preferred == 'ISO-8859-10', Name == 'ISO-8859-10', MIBenum == 13))) :-
		iso_8859_10::preferred_mime_name(Preferred),
		iso_8859_10::name(Name),
		iso_8859_10::mibenum(MIBenum).

	test(iso_8859_13_metadata_01, true((Preferred == 'ISO-8859-13', Name == 'ISO-8859-13', MIBenum == 109))) :-
		iso_8859_13::preferred_mime_name(Preferred),
		iso_8859_13::name(Name),
		iso_8859_13::mibenum(MIBenum).

	test(iso_8859_14_metadata_01, true((Preferred == 'ISO-8859-14', Name == 'ISO-8859-14', MIBenum == 110))) :-
		iso_8859_14::preferred_mime_name(Preferred),
		iso_8859_14::name(Name),
		iso_8859_14::mibenum(MIBenum).

	test(iso_8859_15_metadata_01, true((Preferred == 'ISO-8859-15', Name == 'ISO-8859-15', MIBenum == 111))) :-
		iso_8859_15::preferred_mime_name(Preferred),
		iso_8859_15::name(Name),
		iso_8859_15::mibenum(MIBenum).

	test(iso_8859_16_metadata_01, true((Preferred == 'ISO-8859-16', Name == 'ISO-8859-16', MIBenum == 112))) :-
		iso_8859_16::preferred_mime_name(Preferred),
		iso_8859_16::name(Name),
		iso_8859_16::mibenum(MIBenum).

	test(windows_1250_metadata_01, true((Preferred == 'windows-1250', Name == 'windows-1250', MIBenum == 2250))) :-
		windows_1250::preferred_mime_name(Preferred),
		windows_1250::name(Name),
		windows_1250::mibenum(MIBenum).

	test(windows_1251_metadata_01, true((Preferred == 'windows-1251', Name == 'windows-1251', MIBenum == 2251))) :-
		windows_1251::preferred_mime_name(Preferred),
		windows_1251::name(Name),
		windows_1251::mibenum(MIBenum).

	test(windows_1253_metadata_01, true((Preferred == 'windows-1253', Name == 'windows-1253', MIBenum == 2253))) :-
		windows_1253::preferred_mime_name(Preferred),
		windows_1253::name(Name),
		windows_1253::mibenum(MIBenum).

	test(windows_1254_metadata_01, true((Preferred == 'windows-1254', Name == 'windows-1254', MIBenum == 2254))) :-
		windows_1254::preferred_mime_name(Preferred),
		windows_1254::name(Name),
		windows_1254::mibenum(MIBenum).

	test(windows_1257_metadata_01, true((Preferred == 'windows-1257', Name == 'windows-1257', MIBenum == 2257))) :-
		windows_1257::preferred_mime_name(Preferred),
		windows_1257::name(Name),
		windows_1257::mibenum(MIBenum).

	test(windows_1252_metadata_01, true((Preferred == 'windows-1252', Name == 'windows-1252', MIBenum == 2252))) :-
		windows_1252::preferred_mime_name(Preferred),
		windows_1252::name(Name),
		windows_1252::mibenum(MIBenum).

	test(utf_8_metadata_01, true((Preferred == 'UTF-8', Name == 'UTF-8', MIBenum == 106))) :-
		utf_8::preferred_mime_name(Preferred),
		utf_8::name(Name),
		utf_8::mibenum(MIBenum).

	test(utf_8_metadata_02, true) :-
		utf_8::alias('csUTF8').

	test(utf_16le_metadata_01, true((Preferred == 'UTF-16LE', Name == 'UTF-16LE', MIBenum == 1014))) :-
		utf_16le::preferred_mime_name(Preferred),
		utf_16le::name(Name),
		utf_16le::mibenum(MIBenum).

	test(utf_16be_metadata_01, true((Preferred == 'UTF-16BE', Name == 'UTF-16BE', MIBenum == 1013))) :-
		utf_16be::preferred_mime_name(Preferred),
		utf_16be::name(Name),
		utf_16be::mibenum(MIBenum).

	test(utf_32le_metadata_01, true((Preferred == 'UTF-32LE', Name == 'UTF-32LE', MIBenum == 1019))) :-
		utf_32le::preferred_mime_name(Preferred),
		utf_32le::name(Name),
		utf_32le::mibenum(MIBenum).

	test(utf_32be_metadata_01, true((Preferred == 'UTF-32BE', Name == 'UTF-32BE', MIBenum == 1018))) :-
		utf_32be::preferred_mime_name(Preferred),
		utf_32be::name(Name),
		utf_32be::mibenum(MIBenum).

	test(us_ascii_codes_to_bytes_01, true(Bytes == [65, 66, 67])) :-
		us_ascii::codes_to_bytes([65, 66, 67], Bytes).

	test(us_ascii_codes_to_bytes_02, fail) :-
		us_ascii::codes_to_bytes([128], _).

	test(us_ascii_bytes_to_codes_01, true(Codes == [0, 127])) :-
		us_ascii::bytes_to_codes([0, 127], Codes).

	test(us_ascii_bytes_to_codes_02, fail) :-
		us_ascii::bytes_to_codes([128], _).

	test(iso_8859_1_codes_to_bytes_01, true(Bytes == [0, 65, 255])) :-
		iso_8859_1::codes_to_bytes([0, 65, 255], Bytes).

	test(iso_8859_1_bytes_to_codes_01, true(Codes == [0, 65, 255])) :-
		iso_8859_1::bytes_to_codes([0, 65, 255], Codes).

	test(iso_8859_2_codes_to_bytes_01, true(Bytes == [0xA1, 0xA2, 0xC0, 0xE0])) :-
		iso_8859_2::codes_to_bytes([0x0104, 0x02D8, 0x0154, 0x0155], Bytes).

	test(iso_8859_2_bytes_to_codes_01, true(Codes == [0x0104, 0x02D8, 0x0154, 0x0155])) :-
		iso_8859_2::bytes_to_codes([0xA1, 0xA2, 0xC0, 0xE0], Codes).

	test(iso_8859_3_codes_to_bytes_01, true(Bytes == [0xA1, 0xA6, 0xD5, 0xFE])) :-
		iso_8859_3::codes_to_bytes([0x0126, 0x0124, 0x0120, 0x015D], Bytes).

	test(iso_8859_3_bytes_to_codes_01, true(Codes == [0x0126, 0x0124, 0x0120, 0x015D])) :-
		iso_8859_3::bytes_to_codes([0xA1, 0xA6, 0xD5, 0xFE], Codes).

	test(iso_8859_4_codes_to_bytes_01, true(Bytes == [0xA1, 0xB2, 0xD3, 0xFF])) :-
		iso_8859_4::codes_to_bytes([0x0104, 0x02DB, 0x0136, 0x02D9], Bytes).

	test(iso_8859_4_bytes_to_codes_01, true(Codes == [0x0104, 0x02DB, 0x0136, 0x02D9])) :-
		iso_8859_4::bytes_to_codes([0xA1, 0xB2, 0xD3, 0xFF], Codes).

	test(iso_8859_9_codes_to_bytes_01, true(Bytes == [0xD0, 0xDD, 0xFE])) :-
		iso_8859_9::codes_to_bytes([0x011E, 0x0130, 0x015F], Bytes).

	test(iso_8859_9_bytes_to_codes_01, true(Codes == [0x011E, 0x0130, 0x015F])) :-
		iso_8859_9::bytes_to_codes([0xD0, 0xDD, 0xFE], Codes).

	test(iso_8859_10_codes_to_bytes_01, true(Bytes == [0xA1, 0xBD, 0xFF])) :-
		iso_8859_10::codes_to_bytes([0x0104, 0x2015, 0x0138], Bytes).

	test(iso_8859_10_bytes_to_codes_01, true(Codes == [0x0104, 0x2015, 0x0138])) :-
		iso_8859_10::bytes_to_codes([0xA1, 0xBD, 0xFF], Codes).

	test(iso_8859_13_codes_to_bytes_01, true(Bytes == [0xA1, 0xAF, 0xFF])) :-
		iso_8859_13::codes_to_bytes([0x201D, 0x00C6, 0x2019], Bytes).

	test(iso_8859_13_bytes_to_codes_01, true(Codes == [0x201D, 0x00C6, 0x2019])) :-
		iso_8859_13::bytes_to_codes([0xA1, 0xAF, 0xFF], Codes).

	test(iso_8859_14_codes_to_bytes_01, true(Bytes == [0xA1, 0xD0, 0xFE])) :-
		iso_8859_14::codes_to_bytes([0x1E02, 0x0174, 0x0177], Bytes).

	test(iso_8859_14_bytes_to_codes_01, true(Codes == [0x1E02, 0x0174, 0x0177])) :-
		iso_8859_14::bytes_to_codes([0xA1, 0xD0, 0xFE], Codes).

	test(iso_8859_15_codes_to_bytes_01, true(Bytes == [0xA4, 0xBC, 0xBE])) :-
		iso_8859_15::codes_to_bytes([0x20AC, 0x0152, 0x0178], Bytes).

	test(iso_8859_15_bytes_to_codes_01, true(Codes == [0x20AC, 0x0152, 0x0178])) :-
		iso_8859_15::bytes_to_codes([0xA4, 0xBC, 0xBE], Codes).

	test(iso_8859_16_codes_to_bytes_01, true(Bytes == [0xA4, 0xAA, 0xFE])) :-
		iso_8859_16::codes_to_bytes([0x20AC, 0x0218, 0x021B], Bytes).

	test(iso_8859_16_bytes_to_codes_01, true(Codes == [0x20AC, 0x0218, 0x021B])) :-
		iso_8859_16::bytes_to_codes([0xA4, 0xAA, 0xFE], Codes).

	test(windows_1250_codes_to_bytes_01, true(Bytes == [0x80, 0x8A, 0x8C, 0x9F])) :-
		windows_1250::codes_to_bytes([0x20AC, 0x0160, 0x015A, 0x017A], Bytes).

	test(windows_1250_bytes_to_codes_01, true(Codes == [0x20AC, 0x0160, 0x015A, 0x017A])) :-
		windows_1250::bytes_to_codes([0x80, 0x8A, 0x8C, 0x9F], Codes).

	test(windows_1250_codes_to_bytes_02, fail) :-
		windows_1250::codes_to_bytes([0x81], _).

	test(windows_1250_bytes_to_codes_02, fail) :-
		windows_1250::bytes_to_codes([0x81], _).

	test(windows_1251_codes_to_bytes_01, true(Bytes == [0x80, 0x88, 0x90, 0xFF])) :-
		windows_1251::codes_to_bytes([0x0402, 0x20AC, 0x0452, 0x044F], Bytes).

	test(windows_1251_bytes_to_codes_01, true(Codes == [0x0402, 0x20AC, 0x0452, 0x044F])) :-
		windows_1251::bytes_to_codes([0x80, 0x88, 0x90, 0xFF], Codes).

	test(windows_1251_codes_to_bytes_02, fail) :-
		windows_1251::codes_to_bytes([0x98], _).

	test(windows_1251_bytes_to_codes_02, fail) :-
		windows_1251::bytes_to_codes([0x98], _).

	test(windows_1253_codes_to_bytes_01, true(Bytes == [0x80, 0xA1, 0xD3, 0xFE])) :-
		windows_1253::codes_to_bytes([0x20AC, 0x0385, 0x03A3, 0x03CE], Bytes).

	test(windows_1253_bytes_to_codes_01, true(Codes == [0x20AC, 0x0385, 0x03A3, 0x03CE])) :-
		windows_1253::bytes_to_codes([0x80, 0xA1, 0xD3, 0xFE], Codes).

	test(windows_1253_codes_to_bytes_02, fail) :-
		windows_1253::codes_to_bytes([0x81], _).

	test(windows_1253_bytes_to_codes_02, fail) :-
		windows_1253::bytes_to_codes([0x81], _).

	test(windows_1254_codes_to_bytes_01, true(Bytes == [0x80, 0x8C, 0xD0, 0xFD])) :-
		windows_1254::codes_to_bytes([0x20AC, 0x0152, 0x011E, 0x0131], Bytes).

	test(windows_1254_bytes_to_codes_01, true(Codes == [0x20AC, 0x0152, 0x011E, 0x0131])) :-
		windows_1254::bytes_to_codes([0x80, 0x8C, 0xD0, 0xFD], Codes).

	test(windows_1254_codes_to_bytes_02, fail) :-
		windows_1254::codes_to_bytes([0x81], _).

	test(windows_1254_bytes_to_codes_02, fail) :-
		windows_1254::bytes_to_codes([0x81], _).

	test(windows_1257_codes_to_bytes_01, true(Bytes == [0x80, 0xAA, 0xAF, 0xFE])) :-
		windows_1257::codes_to_bytes([0x20AC, 0x0156, 0x00C6, 0x017E], Bytes).

	test(windows_1257_bytes_to_codes_01, true(Codes == [0x20AC, 0x0156, 0x00C6, 0x017E])) :-
		windows_1257::bytes_to_codes([0x80, 0xAA, 0xAF, 0xFE], Codes).

	test(windows_1257_codes_to_bytes_02, fail) :-
		windows_1257::codes_to_bytes([0x81], _).

	test(windows_1257_bytes_to_codes_02, fail) :-
		windows_1257::bytes_to_codes([0x81], _).

	test(windows_1252_codes_to_bytes_01, true(Bytes == [0x80, 0x8C, 0x9F])) :-
		windows_1252::codes_to_bytes([0x20AC, 0x0152, 0x0178], Bytes).

	test(windows_1252_bytes_to_codes_01, true(Codes == [0x20AC, 0x0152, 0x0178])) :-
		windows_1252::bytes_to_codes([0x80, 0x8C, 0x9F], Codes).

	test(windows_1252_codes_to_bytes_02, fail) :-
		windows_1252::codes_to_bytes([0x81], _).

	test(windows_1252_bytes_to_codes_02, fail) :-
		windows_1252::bytes_to_codes([0x81], _).

	test(utf_8_codes_to_bytes_01, true(Bytes == [0x24, 0xC2, 0xA2, 0xE2, 0x82, 0xAC, 0xF0, 0x90, 0x8D, 0x88])) :-
		sample_codes(Codes),
		utf_8::codes_to_bytes(Codes, Bytes).

	test(utf_8_bytes_to_codes_01, true(Codes == [0x24, 0xA2, 0x20AC, 0x10348])) :-
		utf_8::bytes_to_codes([0x24, 0xC2, 0xA2, 0xE2, 0x82, 0xAC, 0xF0, 0x90, 0x8D, 0x88], Codes).

	test(utf_8_bytes_to_codes_02, fail) :-
		utf_8::bytes_to_codes([0xC0, 0x80], _).

	test(utf_16le_codes_to_bytes_01, true(Bytes == [0x24, 0x00, 0xA2, 0x00, 0xAC, 0x20, 0x00, 0xD8, 0x48, 0xDF])) :-
		sample_codes(Codes),
		utf_16le::codes_to_bytes(Codes, Bytes).

	test(utf_16le_bytes_to_codes_01, true(Codes == [0x24, 0xA2, 0x20AC, 0x10348])) :-
		utf_16le::bytes_to_codes([0x24, 0x00, 0xA2, 0x00, 0xAC, 0x20, 0x00, 0xD8, 0x48, 0xDF], Codes).

	test(utf_16le_bytes_to_codes_02, fail) :-
		utf_16le::bytes_to_codes([0x00, 0xDC], _).

	test(utf_16be_codes_to_bytes_01, true(Bytes == [0x00, 0x24, 0x00, 0xA2, 0x20, 0xAC, 0xD8, 0x00, 0xDF, 0x48])) :-
		sample_codes(Codes),
		utf_16be::codes_to_bytes(Codes, Bytes).

	test(utf_16be_bytes_to_codes_01, true(Codes == [0x24, 0xA2, 0x20AC, 0x10348])) :-
		utf_16be::bytes_to_codes([0x00, 0x24, 0x00, 0xA2, 0x20, 0xAC, 0xD8, 0x00, 0xDF, 0x48], Codes).

	test(utf_32le_codes_to_bytes_01, true(Bytes == [0x24, 0x00, 0x00, 0x00, 0xA2, 0x00, 0x00, 0x00, 0xAC, 0x20, 0x00, 0x00, 0x48, 0x03, 0x01, 0x00])) :-
		sample_codes(Codes),
		utf_32le::codes_to_bytes(Codes, Bytes).

	test(utf_32le_bytes_to_codes_01, true(Codes == [0x24, 0xA2, 0x20AC, 0x10348])) :-
		utf_32le::bytes_to_codes([0x24, 0x00, 0x00, 0x00, 0xA2, 0x00, 0x00, 0x00, 0xAC, 0x20, 0x00, 0x00, 0x48, 0x03, 0x01, 0x00], Codes).

	test(utf_32be_codes_to_bytes_01, true(Bytes == [0x00, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0xA2, 0x00, 0x00, 0x20, 0xAC, 0x00, 0x01, 0x03, 0x48])) :-
		sample_codes(Codes),
		utf_32be::codes_to_bytes(Codes, Bytes).

	test(utf_32be_bytes_to_codes_01, true(Codes == [0x24, 0xA2, 0x20AC, 0x10348])) :-
		utf_32be::bytes_to_codes([0x00, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0xA2, 0x00, 0x00, 0x20, 0xAC, 0x00, 0x01, 0x03, 0x48], Codes).

	test(utf_32be_bytes_to_codes_02, fail) :-
		utf_32be::bytes_to_codes([0x00, 0x00, 0xD8, 0x00], _).

	test(utf_32be_codes_to_bytes_02, fail) :-
		utf_32be::codes_to_bytes([0xD800], _).

	sample_codes([0x24, 0xA2, 0x20AC, 0x10348]).

:- end_object.
