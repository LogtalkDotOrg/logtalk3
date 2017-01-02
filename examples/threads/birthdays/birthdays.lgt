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


:- object(agent,
	implements(monitoring)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura and Peter Robinson',
		date is 2007/3/24,
		comment is 'Simple multi-threading example with agents and their birthdays.'
	]).

	:- threaded.

	:- public(new/3).
	:- mode(new(+atom, +integer, +atom), one).
	:- info(new/3, [
		comment is 'Creates a new agent given its name, age, and gender.',
		argnames is ['Name', 'Age', 'Gender']
	]).

	:- public(age/1).
	:- dynamic(age/1).
	:- mode(age(?integer), zero_or_one).
	:- info(age/1, [
		comment is 'Agent age.'
	]).

	:- public(gender/1).
	:- dynamic(gender/1).
	:- mode(gender(?integer), zero_or_one).
	:- info(gender/1, [
		comment is 'Agent gender.'
	]).

	:- public(birthday/0).
	:- mode(birthday, one).
	:- info(birthday/0, [
		comment is 'Increments an agent age, an unfortunate side-effect of its birthday.'
	]).

	:- public(happy_birthday/1).
	:- mode(happy_birthday(+object_identifier), one).
	:- info(happy_birthday/1, [
		comment is 'Happy birthday message from a friend.',
		argnames is ['From']
	]).

	:- public(cake_slice/1).
	:- mode(cake_slice(+object_identifier), one).
	:- info(cake_slice/1, [
		comment is 'Offer a slice of birthday cake to a friend.',
		argnames is ['From']
	]).

	:- public(new_friend/1).
	:- mode(new_friend(+object_identifier), one).
	:- info(new_friend/1, [
		comment is 'New friend, watch out for his/her birthday.',
		argnames is ['Name']
	]).

	% new agents are created as multi-threading enabled objects:
	new(Name, Age, Gender) :-
		this(This),
		create_object(Name, [extends(This)], [threaded], [age(Age), gender(Gender)]).

	% getting older:
	birthday :-
		::retract(age(Old)),
		New is Old + 1,
		::assertz(age(New)).

	% when someone congratulate us for our birthday, acknowledge it,
	% and offer her/him a slice of the birthday cake:
	happy_birthday(From) :-
		self(Self),
		write('Happy birthday from '), write(From), write('!'), nl,
		write('Thanks! Here, have a slice of cake, '), write(From), write('.'), nl,
		threaded_ignore(From::cake_slice(Self)).	% we don't care what happens with the cake slice

	% be nice, give thanks when someone offer us a slice of cake:
	cake_slice(From) :-
		write('Thanks for the cake '), write(From), write('!'), nl,
		threaded_exit(From::age(Age)),				% retrieve our friend's (old) age
		write('Say goodbye to your '), write(Age), write('''s'), write('!'), nl,
		threaded_once(From::age(_)).				% get (new) age for the next anniversary!

	% watch out for our new friend anniversary and find out his/her age:
	new_friend(Friend) :-
		self(Self),
		define_events(after, Friend, birthday, _, Self),
		threaded_once(Friend::age(_)).

	% congratule a friend for his/her birthday:
	after(Friend, birthday, _) :-
		self(Self),
		threaded_ignore(Friend::happy_birthday(Self)).

:- end_object.
