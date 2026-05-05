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
		comment is 'Scoped unit tests for the "iso_639" library scaffolding.'
	]).

	cover(iso_639).
	cover(iso_639_1).
	cover(iso_639_2).
	cover(iso_639_3).
	cover(iso_639_5).

	test(language_03_01, deterministic(Alpha3-Name == eng-'English')) :-
		iso_639::language(en, Alpha3, Name).

	test(language_03_02, deterministic(Alpha3-Name == sqi-'Albanian')) :-
		iso_639::language(sq, Alpha3, Name).

	test(language_03_03, fail) :-
		iso_639::language(zz, _, _).

	test(language_code_05_01, deterministic(Terminologic-Alpha2-Name-Class == sqi-sq-'Albanian'-macrolanguage)) :-
		iso_639::language_code(alb, Terminologic, Alpha2, Name, Class).

	test(language_code_05_02, deterministic(Class == collective)) :-
		iso_639::language_code(afa, _, _, 'Afro-Asiatic languages', Class).

	test(language_code_05_03, fail) :-
		iso_639::language_code(zzz, zzz, zz, _, _).

	test(language_05_01, deterministic(Alpha2-Scope-Type-Name == en-individual-living-'English')) :-
		iso_639::language(eng, Alpha2, Scope, Type, Name).

	test(language_05_02, deterministic(Alpha2-Scope-Type-Name == sq-macrolanguage-living-'Albanian')) :-
		iso_639::language(sqi, Alpha2, Scope, Type, Name).

	test(language_05_03, fail) :-
		iso_639::language(zzz, _, _, _, _).

	test(language_group_02_01, deterministic(Name == 'Romance languages')) :-
		iso_639::language_group(roa, Name).

	test(language_group_02_02, fail) :-
		iso_639::language_group(zzz, _).

:- end_object.
