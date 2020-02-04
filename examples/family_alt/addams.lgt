%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(addams,
	implements(basic_family_relations)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2017-03-06,
		comment is 'Addams family.'
	]).

	% register this family by providing linking
	% clauses for the family basic relations

	:- multifile(family(_)::male/2).
	family(_)::male(addams, Male) :-
		male(Male).

	:- multifile(family(_)::female/2).
	family(_)::female(addams, Male) :-
		female(Male).

	:- multifile(family(_)::parent/3).
	family(_)::parent(addams, Parent, Child) :-
		parent(Parent, Child).

	% define the family basic relations

	male(gomez).
	male(pubert).
	male(pugsley).

	female(morticia).
	female(wednesday).

	parent(gomez, pubert).
	parent(gomez, pugsley).
	parent(gomez, wednesday).
	parent(morticia, pubert).
	parent(morticia, pugsley).
	parent(morticia, wednesday).

:- end_object.
