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


:- object(data_acquisition).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2019/11/21,
		comment is 'Data acquisition example, which is fully decoupled from data processing details.'
	]).

	:- public(parents/3).
	:- mode(parents(?atom, --expected, --expected), zero_or_more).
	:- info(parents/3, [
		comment is 'Enumerates, by backtracking, all persons and their parents.',
		argnames is ['Child', 'ExpectedFather', 'ExpectedMother']
	]).

	% we reuse for this example the database on the Addams family
	% facts included in the "family" example
	:- uses(addams, [
		male/1, female/1, parent/2
	]).

	parents(Person, ExpectedFather, ExpectedMother) :-
		% enumerate the persons in the family database
		(	male(Person)
		;	female(Person)
		),
		% we expect the names of the person parents to be known but
		% there might be some missing data; we delegate handling these
		% cases to the code that processes the data by using expected
		% terms that either hold the parent name or error information
		expected::from_goal((parent(Father,Person), male(Father)), Father, missing_father, ExpectedFather),
		expected::from_goal((parent(Mother,Person), female(Mother)), Mother, missing_mother, ExpectedMother).

:- end_object.


:- object(data_processing).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2018/06/10,
		comment is 'Data processing example, which is fully decoupled from data acquisition details.'
	]).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0, [
		comment is 'Prints the names of all persons and their parents.'
	]).

	:- public(print_complete/0).
	:- mode(print_complete, one).
	:- info(print_complete/0, [
		comment is 'Prints the names of all persons that have know parents.'
	]).

	:- public(check/0).
	:- mode(check, one).
	:- info(check/0, [
		comment is 'Checks that all persons have know parents. Throws an error otherwise.'
	]).

	print :-
		forall(
			(	data_acquisition::parents(Person, ExpectedFather, ExpectedMother),
				% replace missing father or mother with hypothetical names
				expected(ExpectedFather)::or_else(Father, 'john doe'),
				expected(ExpectedMother)::or_else(Mother, 'jane doe')
			),
			print(Person, Father, Mother)
		).

	print_complete :-
		forall(
			(	data_acquisition::parents(Person, ExpectedFather, ExpectedMother),
				% fail if the father or the mother are unknown
				expected(ExpectedFather)::or_else_fail(Father),
				expected(ExpectedMother)::or_else_fail(Mother)
			),
			print(Person, Father, Mother)
		).

	check :-
		forall(
			data_acquisition::parents(Person, ExpectedFather, ExpectedMother),
			% throw an error if the expected father or mother names are missing
			(	expected(ExpectedFather)::if_unexpected({Person}/[Error]>>throw(Error-Person)),
				expected(ExpectedMother)::if_unexpected({Person}/[Error]>>throw(Error-Person))
			)
		).

	print(Person, Father, Mother) :-
		write(Person), nl,
		write('  father: '), write(Father), nl,
		write('  mother: '), write(Mother), nl, nl.

:- end_object.
