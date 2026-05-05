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


:- protocol(iso_9362_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Protocol for ISO 9362 BIC structure parsing and normalization.'
	]).

	:- public(bic/5).
	:- mode(bic(+atom, ?atom, ?atom, ?atom, ?atom), zero_or_one).
	:- info(bic/5, [
		comment is 'Parses and validates an ISO 9362 BIC atom into business party prefix, ISO 3166-1 alpha-2 country code, business party suffix, and branch identifier. Eight-character BICs are normalized to the primary-office branch code ``XXX``.',
		argnames is ['BIC', 'Prefix', 'CountryAlpha2', 'Suffix', 'Branch']
	]).

	:- public(canonical_bic/2).
	:- mode(canonical_bic(+atom, ?atom), zero_or_one).
	:- info(canonical_bic/2, [
		comment is 'Returns the canonical 11-character form of a valid ISO 9362 BIC, using the branch code ``XXX`` when the input is an eight-character primary-office BIC.',
		argnames is ['BIC', 'Canonical']
	]).

	:- public(primary_office_bic/1).
	:- mode(primary_office_bic(+atom), zero_or_one).
	:- info(primary_office_bic/1, [
		comment is 'True when a valid ISO 9362 BIC denotes the primary office, either because it is eight characters long or because its branch identifier is ``XXX``.',
		argnames is ['BIC']
	]).

:- end_protocol.
