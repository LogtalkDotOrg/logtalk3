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


:- object(simpsons,
	implements(basic_family_relations)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2017-03-06,
		comment is 'Simpsons family.'
	]).

	% register this family by providing linking
	% clauses for the family basic relations

	:- multifile(family(_)::male/2).
	family(_)::male(simpsons, Male) :-
		male(Male).

	:- multifile(family(_)::female/2).
	family(_)::female(simpsons, Male) :-
		female(Male).

	:- multifile(family(_)::parent/3).
	family(_)::parent(simpsons, Parent, Child) :-
		parent(Parent, Child).

	% define the family basic relations

	male(homer).
	male(bart).

	female(lisa).
	female(maggie).
	female(marge).

	parent(homer, bart).
	parent(homer, lisa).
	parent(homer, maggie).
	parent(marge, bart).
	parent(marge, lisa).
	parent(marge, maggie).

:- end_object.
