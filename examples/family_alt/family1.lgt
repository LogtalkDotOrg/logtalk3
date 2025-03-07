%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(family(_Family_),
	implements(basic_family_relations)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-02-07,
		comment is 'Family relations.',
		parnames is ['Family']
	]).

	% extended family relations

	:- public([
		father/2, mother/2,
		sister/2, brother/2
	]).

	% entry points to register concrete families

	:- private(parent/3).
	:- multifile(parent/3).

	:- private(male/2).
	:- multifile(male/2).

	:- private(female/2).
	:- multifile(female/2).

	% delegate queries about basic family relations
	% to the registered concrete families

	male(Male) :-
		male(_Family_, Male).

	female(Female) :-
		female(_Family_, Female).

	parent(Parent, Child) :-
		parent(_Family_, Parent, Child).

	% define the extended family relations

	father(Father, Child) :-
		male(Father),
		parent(Father, Child).

	mother(Mother, Child) :-
		female(Mother),
		parent(Mother, Child).

	sister(Sister, Child) :-
		female(Sister),
		parent(Parent, Sister),
		parent(Parent, Child),
		Sister \== Child.

	brother(Brother, Child) :-
		male(Brother),
		parent(Parent, Brother),
		parent(Parent, Child),
		Brother \== Child.

:- end_object.
