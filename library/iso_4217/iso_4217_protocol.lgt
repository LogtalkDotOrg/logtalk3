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


:- protocol(iso_4217_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Protocol for ISO 4217 active currency and fund code lookups.'
	]).

	:- public(currency/5).
	:- mode(currency(?atom, ?integer, ?term, ?atom, ?atom), zero_or_more).
	:- info(currency/5, [
		comment is 'Enumerates ISO 4217 active non-fund entries as alphabetic code, numeric code, minor unit, currency name, and entity tuples. The minor unit is either an integer or the atom ``na`` when the standard lists ``N.A.``.',
		argnames is ['Alphabetic', 'Numeric', 'MinorUnit', 'Name', 'Entity']
	]).

	:- public(fund_currency/5).
	:- mode(fund_currency(?atom, ?integer, ?term, ?atom, ?atom), zero_or_more).
	:- info(fund_currency/5, [
		comment is 'Enumerates ISO 4217 active fund entries as alphabetic code, numeric code, minor unit, currency name, and entity tuples. The minor unit is either an integer or the atom ``na`` when the standard lists ``N.A.``.',
		argnames is ['Alphabetic', 'Numeric', 'MinorUnit', 'Name', 'Entity']
	]).

:- end_protocol.
