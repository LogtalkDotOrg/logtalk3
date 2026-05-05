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
		comment is 'Scoped unit tests for the "iso_3166" library.'
	]).

	cover(iso_3166).
	cover(iso_3166_1).
	cover(iso_3166_2).

	test(country_04_01, deterministic(Alpha3-Numeric-Name == afg-4-'Afghanistan')) :-
		iso_3166::country(af, Alpha3, Numeric, Name).

	test(country_04_02, deterministic(Alpha2-Alpha3-Numeric == us-usa-840)) :-
		iso_3166::country(Alpha2, Alpha3, Numeric, 'United States of America').

	test(country_04_03, fail) :-
		iso_3166::country(zz, _, _, _).

	test(subdivision_04_01, fail) :-
		iso_3166::subdivision('ZZ-XXX', _, _, _).

	test(subdivision_04_02, deterministic(CountryAlpha2-Name-Category == pt-'Aveiro'-'District')) :-
		iso_3166::subdivision('pt-01', CountryAlpha2, Name, Category).

	test(subdivision_04_03, deterministic(Code-CountryAlpha2-Category == 'us-ca'-us-'State')) :-
		iso_3166::subdivision(Code, CountryAlpha2, 'California', Category).

:- end_object.
