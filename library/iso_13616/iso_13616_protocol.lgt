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


:- protocol(iso_13616_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Protocol for ISO 13616 IBAN structure parsing, checksum validation, and normalization.'
	]).

	:- public(iban/4).
	:- mode(iban(+atom, ?atom, ?atom, ?atom), zero_or_one).
	:- info(iban/4, [
		comment is 'Parses and validates an ISO 13616 IBAN atom into ISO 3166-1 alpha-2 country code, two-character check digits, and BBAN atom. Input may use lowercase letters and embedded spaces; successful results are normalized to uppercase compact components.',
		argnames is ['IBAN', 'CountryAlpha2', 'CheckDigits', 'BBAN']
	]).

	:- public(canonical_iban/2).
	:- mode(canonical_iban(+atom, ?atom), zero_or_one).
	:- info(canonical_iban/2, [
		comment is 'Returns the canonical electronic-format IBAN for a valid input by removing spaces and converting letters to uppercase.',
		argnames is ['IBAN', 'Canonical']
	]).

	:- public(formatted_iban/2).
	:- mode(formatted_iban(+atom, ?atom), zero_or_one).
	:- info(formatted_iban/2, [
		comment is 'Returns a valid IBAN formatted in groups of four characters separated by spaces.',
		argnames is ['IBAN', 'Formatted']
	]).

:- end_protocol.
