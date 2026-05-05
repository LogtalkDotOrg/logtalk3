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


:- protocol(iso_3166_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Protocol for ISO 3166 country and subdivision registry lookups.'
	]).

	:- public(country/4).
	:- mode(country(?atom, ?atom, ?integer, ?atom), zero_or_more).
	:- info(country/4, [
		comment is 'Enumerates ISO 3166-1 country entries as alpha-2, alpha-3, numeric, and English short name tuples.',
		argnames is ['Alpha2', 'Alpha3', 'Numeric', 'Name']
	]).

	:- public(subdivision/4).
	:- mode(subdivision(?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(subdivision/4, [
		comment is 'Enumerates ISO 3166-2 subdivision entries as code, parent alpha-2 country code, name, and category tuples.',
		argnames is ['Code', 'CountryAlpha2', 'Name', 'Category']
	]).

:- end_protocol.
