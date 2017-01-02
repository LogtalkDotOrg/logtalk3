%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/09/23,
		comment is 'Unit tests for the "family" example.'
	]).

	test(family_1) :-
		setof(Female, addams::female(Female), Females),
		Females == [morticia, wednesday].

	test(family_2) :-
		setof(Male, addams::male(Male), Males),
		Males == [gomez, pubert, pugsley].

	test(family_3) :-
		setof(Child, addams::mother(Mother, Child), Childs),
		Mother == morticia, Childs == [pubert, pugsley, wednesday].

	test(family_4) :-
		setof(Child, simpsons::father(Father, Child), Childs),
		Father == homer, Childs == [bart, lisa, maggie].

	test(family_5) :-
		setof(Male, simpsons_extended::male(Male), Males),
		Males == [abe, bart, herb, homer].

:- end_object.
