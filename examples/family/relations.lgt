%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(familytree).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/09/23,
		comment is 'Family relations.'
	]).

	:- public([
		father/2, mother/2,
		sister/2, brother/2
	]).

	:- public([
		parent/2,
		male/1, female/1
	]).

	father(Father, Child) :-
		::male(Father),
		::parent(Father, Child).

	mother(Mother, Child) :-
		::female(Mother),
		::parent(Mother, Child).

	sister(Sister, Child) :-
		::female(Sister),
		::parent(Parent, Sister),
		::parent(Parent, Child),
		Sister \== Child.

	brother(Brother, Child) :-
		::male(Brother),
		::parent(Parent, Brother),
		::parent(Parent, Child),
		Brother \== Child.

:- end_object.
