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
		date is 2026-05-05,
		comment is 'Scoped unit tests for the "iso_13616" library.'
	]).

	cover(iso_13616).

	test(iban_04_01, deterministic(CountryAlpha2-CheckDigits-BBAN == 'GB'-'82'-'WEST12345698765432')) :-
		iso_13616::iban('GB82WEST12345698765432', CountryAlpha2, CheckDigits, BBAN).

	test(iban_04_02, deterministic(CountryAlpha2-CheckDigits-BBAN == 'GB'-'82'-'WEST12345698765432')) :-
		iso_13616::iban('gb82 west 1234 5698 7654 32', CountryAlpha2, CheckDigits, BBAN).

	test(canonical_iban_02_01, deterministic(Canonical == 'GB82WEST12345698765432')) :-
		iso_13616::canonical_iban('gb82 west 1234 5698 7654 32', Canonical).

	test(formatted_iban_02_01, deterministic(Formatted == 'GB82 WEST 1234 5698 7654 32')) :-
		iso_13616::formatted_iban('GB82WEST12345698765432', Formatted).

	test(iban_04_03, deterministic(CountryAlpha2-CheckDigits-BBAN == 'NL'-'91'-'ABNA0417164300')) :-
		iso_13616::iban('NL91ABNA0417164300', CountryAlpha2, CheckDigits, BBAN).

	test(iban_04_04, deterministic(CountryAlpha2-CheckDigits-BBAN == 'FR'-'14'-'20041010050500013M02606')) :-
		iso_13616::iban('FR1420041010050500013M02606', CountryAlpha2, CheckDigits, BBAN).

	test(iban_04_05, fail) :-
		iso_13616::iban('GB81WEST12345698765432', _, _, _).

	test(iban_04_06, fail) :-
		iso_13616::iban('ZZ82WEST12345698765432', _, _, _).

	test(iban_04_07, fail) :-
		iso_13616::iban('GB82WEST12345698$65432', _, _, _).

	test(iban_04_08, fail) :-
		iso_13616::iban('GB82WEST123456987654321234567890123', _, _, _).

	test(iban_04_09, fail) :-
		iso_13616::iban('DE89ABCD00440532013000', _, _, _).

	test(iban_04_10, fail) :-
		iso_13616::iban('GB82WEST123456987654321', _, _, _).

	test(iban_04_11, fail) :-
		iso_13616::iban('US00ABCDEF12345678901234', _, _, _).

:- end_object.
