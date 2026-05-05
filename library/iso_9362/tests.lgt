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
		comment is 'Scoped unit tests for the "iso_9362" library.'
	]).

	cover(iso_9362).

	test(bic_05_01, deterministic(Prefix-CountryAlpha2-Suffix-Branch == 'DEUT'-'DE'-'FF'-'XXX')) :-
		iso_9362::bic('DEUTDEFF', Prefix, CountryAlpha2, Suffix, Branch).

	test(bic_05_02, deterministic(Branch == '500')) :-
		iso_9362::bic('DEUTDEFF500', 'DEUT', 'DE', 'FF', Branch).

	test(bic_05_03, deterministic(Prefix-CountryAlpha2-Suffix-Branch == 'AB1C'-'GB'-'2L'-'XXX')) :-
		iso_9362::bic('AB1CGB2L', Prefix, CountryAlpha2, Suffix, Branch).

	test(canonical_bic_02_01, deterministic(Canonical == 'DEUTDEFFXXX')) :-
		iso_9362::canonical_bic('DEUTDEFF', Canonical).

	test(primary_office_bic_01_01) :-
		iso_9362::primary_office_bic('DEUTDEFF').

	test(primary_office_bic_01_02) :-
		iso_9362::primary_office_bic('DEUTDEFFXXX').

	test(primary_office_bic_01_03, fail) :-
		iso_9362::primary_office_bic('DEUTDEFF500').

	test(bic_05_04, fail) :-
		iso_9362::bic('DEUTZZFF', _, _, _, _).

	test(bic_05_05, fail) :-
		iso_9362::bic('deutdeff', _, _, _, _).

	test(bic_05_06, fail) :-
		iso_9362::bic('DEUTDE', _, _, _, _).

:- end_object.
