%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2017-03-06,
		comment is 'Unit tests for the "family" example.'
	]).

	test(family_1) :-
		setof(Female, family(addams)::female(Female), Females),
		Females == [morticia, wednesday].

	test(family_2) :-
		setof(Male, family(addams)::male(Male), Males),
		Males == [gomez, pubert, pugsley].

	test(family_3) :-
		setof(Child, family(addams)::mother(Mother, Child), Children),
		Mother == morticia, Children == [pubert, pugsley, wednesday].

	test(family_4) :-
		setof(Child, family(simpsons)::father(Father, Child), Children),
		Father == homer, Children == [bart, lisa, maggie].

	test(family_5) :-
		setof(Male, family(simpsons_extended)::male(Male), Males),
		Males == [abe, bart, herb, homer].

:- end_object.
