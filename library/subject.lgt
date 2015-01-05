%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- category(subject).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2003/02/09,
		comment is 'Smalltalk dependent handling predicates.'
	]).

	:- public(changed/0).
	:- mode(changed, one).
	:- info(changed/0, [
		comment is 'Receiver changed in some way. Notify all dependents.'
	]).

	:- public(changed/1).
	:- mode(changed(?nonvar), one).
	:- info(changed/1, [
		comment is 'Receiver changed as specified in the argument. Notify all dependents.',
		argnames is ['Change']
	]).

	:- public(dependents/1).
	:- mode(dependents(-list), one).
	:- info(dependents/1, [
		comment is 'Returns a list of all dependent objects.',
		argnames is ['Dependents']
	]).

	:- private(dependent_/1).
	:- dynamic(dependent_/1).
	:- mode(dependent_(?object), zero_or_more).
	:- info(dependent_/1, [
		comment is 'Table of dependent objects.',
		argnames is ['Dependent']
	]).

	:- public(addDependent/1).
	:- mode(addDependent(@object), one).
	:- info(addDependent/1, [
		comment is 'Adds a new dependent object.',
		argnames is ['Dependent']
	]).

	:- public(removeDependent/1).
	:- mode(removeDependent(?object), zero_or_more).
	:- info(removeDependent/1, [
		comment is 'Removes a dependent object.',
		argnames is ['Dependent']
	]).

	changed :-
		self(Self),
		forall(::dependent_(Dependent), Dependent::update(Self)).

	changed(Change) :-
		forall(::dependent_(Dependent), Dependent::update(Change)).

	dependents(Dependents) :-
		findall(Dependent, ::dependent_(Dependent), Dependents).

	addDependent(Dependent) :-
		(	::dependent_(Dependent) ->
			true
		;	::asserta(dependent_(Dependent))
		).

	removeDependent(Dependent) :-
		::retract(dependent_(Dependent)).

:- end_category.
