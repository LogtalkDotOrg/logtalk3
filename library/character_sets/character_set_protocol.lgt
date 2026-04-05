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


:- protocol(character_set_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'Character set protocol for converting between lists of character codes and lists of bytes.',
		remarks is [
			'Object names' - 'Concrete object names are derived from the preferred IANA MIME names by lowercasing them and replacing hyphens with underscores.',
			'IANA registry metadata' - 'The metadata predicates reflect the IANA character set registry. When the registry does not define a distinct preferred MIME alias, the ``preferred_mime_name/1`` and ``name/1`` predicates return the same atom.',
			'Unicode encodings' - 'Unicode character sets use Unicode scalar values and do not emit or consume a byte order mark (BOM).'
		]
	]).

	:- public(preferred_mime_name/1).
	:- mode(preferred_mime_name(?atom), zero_or_one).
	:- info(preferred_mime_name/1, [
		comment is 'Preferred MIME name for the character set according to the IANA registry.',
		argnames is ['Name']
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_one).
	:- info(name/1, [
		comment is 'Registered character set name according to the IANA registry.',
		argnames is ['Name']
	]).

	:- public(alias/1).
	:- mode(alias(?atom), zero_or_more).
	:- info(alias/1, [
		comment is 'Alias for the character set according to the IANA registry.',
		argnames is ['Alias']
	]).

	:- public(mibenum/1).
	:- mode(mibenum(?integer), zero_or_one).
	:- info(mibenum/1, [
		comment is 'MIBenum value for the character set according to the IANA registry.',
		argnames is ['MIBenum']
	]).

	:- public(codes_to_bytes/2).
	:- mode(codes_to_bytes(+list(integer), --list(byte)), zero_or_one).
	:- info(codes_to_bytes/2, [
		comment is 'Converts a list of character codes to the corresponding list of bytes in the character set when all codes are representable.',
		argnames is ['Codes', 'Bytes']
	]).

	:- public(bytes_to_codes/2).
	:- mode(bytes_to_codes(+list(byte), --list(integer)), zero_or_one).
	:- info(bytes_to_codes/2, [
		comment is 'Converts a list of bytes in the character set to the corresponding list of character codes when the byte sequence is valid for that character set.',
		argnames is ['Bytes', 'Codes']
	]).

:- end_protocol.
