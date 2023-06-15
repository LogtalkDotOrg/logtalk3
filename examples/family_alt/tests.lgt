%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the "family_alt" example.'
	]).

	test(family_alt_01, true(Females == [morticia, wednesday])) :-
		setof(Female, family(addams)::female(Female), Females).

	test(family_alt_02, true(Males == [gomez, pubert, pugsley])) :-
		setof(Male, family(addams)::male(Male), Males).

	test(family_alt_03, true(Mother-Children == morticia-[pubert, pugsley, wednesday])) :-
		setof(Child, family(addams)::mother(Mother, Child), Children).

	test(family_alt_04, true(Father-Children == homer-[bart, lisa, maggie])) :-
		setof(Child, family(simpsons)::father(Father, Child), Children).

	test(family_alt_05, true(Males == [abe, bart, herb, homer])) :-
		setof(Male, family(simpsons_extended)::male(Male), Males).

:- end_object.
