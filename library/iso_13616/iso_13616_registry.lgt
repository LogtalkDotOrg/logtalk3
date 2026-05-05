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


:- object(iso_13616_registry).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Public SWIFT IBAN registry snapshot facts for ISO 13616 country-specific validation using a derived Prolog BBAN segment representation.'
	]).

	:- public(country_spec/3).
	:- mode(country_spec(?atom, ?integer, ?list), zero_or_more).
	:- info(country_spec/3, [
		comment is 'Maps an IBAN country code to the registered total IBAN length and a derived BBAN structure term. BBANPattern is a list of ``Type-Count`` pairs, where Type is ``a`` for uppercase letters, ``n`` for digits, and ``c`` for uppercase alphanumeric characters. For example, ``[a-4, n-6, n-8]`` denotes four letters followed by six digits and then eight digits.',
		argnames is ['CountryAlpha2', 'IBANLength', 'BBANPattern']
	]).

	country_spec('AD', 24, [n-4, n-4, c-12]).
	country_spec('AE', 23, [n-3, n-16]).
	country_spec('AL', 28, [n-8, c-16]).
	country_spec('AT', 20, [n-5, n-11]).
	country_spec('AZ', 28, [a-4, c-20]).
	country_spec('BA', 20, [n-3, n-3, n-8, n-2]).
	country_spec('BE', 16, [n-3, n-7, n-2]).
	country_spec('BG', 22, [a-4, n-4, n-2, c-8]).
	country_spec('BH', 22, [a-4, c-14]).
	country_spec('BI', 27, [n-5, n-5, n-11, n-2]).
	country_spec('BR', 29, [n-8, n-5, n-10, a-1, c-1]).
	country_spec('BY', 28, [c-4, n-4, c-16]).
	country_spec('CH', 21, [n-5, c-12]).
	country_spec('CR', 22, [n-4, n-14]).
	country_spec('CY', 28, [n-3, n-5, c-16]).
	country_spec('CZ', 24, [n-4, n-6, n-10]).
	country_spec('DE', 22, [n-8, n-10]).
	country_spec('DJ', 27, [n-5, n-5, n-11, n-2]).
	country_spec('DK', 18, [n-4, n-9, n-1]).
	country_spec('DO', 28, [c-4, n-20]).
	country_spec('EE', 20, [n-2, n-14]).
	country_spec('EG', 29, [n-4, n-4, n-17]).
	country_spec('ES', 24, [n-4, n-4, n-1, n-1, n-10]).
	country_spec('FI', 18, [n-3, n-11]).
	country_spec('FK', 18, [a-2, n-12]).
	country_spec('FO', 18, [n-4, n-9, n-1]).
	country_spec('FR', 27, [n-5, n-5, c-11, n-2]).
	country_spec('GB', 22, [a-4, n-6, n-8]).
	country_spec('GE', 22, [a-2, n-16]).
	country_spec('GI', 23, [a-4, c-15]).
	country_spec('GL', 18, [n-4, n-9, n-1]).
	country_spec('GR', 27, [n-3, n-4, c-16]).
	country_spec('GT', 28, [c-4, c-20]).
	country_spec('HN', 28, [a-4, n-20]).
	country_spec('HR', 21, [n-7, n-10]).
	country_spec('HU', 28, [n-3, n-4, n-1, n-15, n-1]).
	country_spec('IE', 22, [a-4, n-6, n-8]).
	country_spec('IL', 23, [n-3, n-3, n-13]).
	country_spec('IQ', 23, [a-4, n-3, n-12]).
	country_spec('IS', 26, [n-4, n-2, n-6, n-10]).
	country_spec('IT', 27, [a-1, n-5, n-5, c-12]).
	country_spec('JO', 30, [a-4, n-4, c-18]).
	country_spec('KW', 30, [a-4, c-22]).
	country_spec('KZ', 20, [n-3, c-13]).
	country_spec('LB', 28, [n-4, c-20]).
	country_spec('LC', 32, [a-4, c-24]).
	country_spec('LI', 21, [n-5, c-12]).
	country_spec('LT', 20, [n-5, n-11]).
	country_spec('LU', 20, [n-3, c-13]).
	country_spec('LV', 21, [a-4, c-13]).
	country_spec('LY', 25, [n-3, n-3, n-15]).
	country_spec('MC', 27, [n-5, n-5, c-11, n-2]).
	country_spec('MD', 24, [c-2, c-18]).
	country_spec('ME', 22, [n-3, n-13, n-2]).
	country_spec('MK', 19, [n-3, c-10, n-2]).
	country_spec('MN', 20, [n-4, n-12]).
	country_spec('MR', 27, [n-5, n-5, n-11, n-2]).
	country_spec('MT', 31, [a-4, n-5, c-18]).
	country_spec('MU', 30, [a-4, n-2, n-2, n-12, n-3, a-3]).
	country_spec('NI', 28, [a-4, n-20]).
	country_spec('NL', 18, [a-4, n-10]).
	country_spec('NO', 15, [n-4, n-6, n-1]).
	country_spec('OM', 23, [n-3, c-16]).
	country_spec('PK', 24, [a-4, c-16]).
	country_spec('PL', 28, [n-8, n-16]).
	country_spec('PS', 29, [a-4, c-21]).
	country_spec('PT', 25, [n-4, n-4, n-11, n-2]).
	country_spec('QA', 29, [a-4, c-21]).
	country_spec('RO', 24, [a-4, c-16]).
	country_spec('RS', 22, [n-3, n-13, n-2]).
	country_spec('RU', 33, [n-9, n-5, c-15]).
	country_spec('SA', 24, [n-2, c-18]).
	country_spec('SC', 31, [a-4, n-2, n-2, n-16, a-3]).
	country_spec('SD', 18, [n-2, n-12]).
	country_spec('SE', 24, [n-3, n-16, n-1]).
	country_spec('SI', 19, [n-5, n-8, n-2]).
	country_spec('SK', 24, [n-4, n-6, n-10]).
	country_spec('SM', 27, [a-1, n-5, n-5, c-12]).
	country_spec('SO', 23, [n-4, n-3, n-12]).
	country_spec('ST', 25, [n-4, n-4, n-11, n-2]).
	country_spec('SV', 28, [a-4, n-20]).
	country_spec('TL', 23, [n-3, n-14, n-2]).
	country_spec('TN', 24, [n-2, n-3, n-13, n-2]).
	country_spec('TR', 26, [n-5, n-1, c-16]).
	country_spec('UA', 29, [n-6, c-19]).
	country_spec('VA', 22, [n-3, n-15]).
	country_spec('VG', 24, [a-4, n-16]).
	country_spec('XK', 20, [n-4, n-10, n-2]).
	country_spec('YE', 30, [a-4, n-4, c-18]).

:- end_object.
