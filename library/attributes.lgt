%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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
%  Public License 3. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- category(attributes).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Dynamic attributes dictionary.']).

	:- public(attribute/2).
	:- mode(attribute(?nonvar, ?nonvar), zero_or_more).
	:- info(attribute/2,
		[comment is 'Returns, by backtracking, all pairs of attribute-values.',
		 argnames is ['Attribute', 'Value']]).

	:- public(attributes/1).
	:- mode(attributes(-list), one).
	:- info(attributes/1,
		[comment is 'List of all pairs of attribute-values.',
		 argnames is ['Attributes']]).

	:- private(attribute_/2).
	:- dynamic(attribute_/2).
	:- mode(attribute_(?nonvar, ?nonvar), zero_or_more).
	:- info(attribute_/2,
		[comment is 'Stores attributes values.',
		 argnames is ['Attribute', 'Value']]).

	:- public(del_attribute/2).
	:- mode(del_attribute(?nonvar, ?nonvar), zero_or_more).
	:- info(del_attribute/2,
		[comment is 'Deletes a matching attribute-value pair.',
		 argnames is ['Attribute', 'Value']]).

	:- public(del_attributes/2).
	:- mode(del_attributes(@term, @term), one).
	:- info(del_attributes/2,
		[comment is 'Deletes all matching attribute-value pairs.',
		 argnames is ['Attribute', 'Value']]).

	:- public(set_attribute/2).
	:- mode(set_attribute(+nonvar, +nonvar), one).
	:- info(set_attribute/2,
		[comment is 'Sets an attribute value.',
		 argnames is ['Attribute', 'Value']]).

	:- public(set_attributes/1).
	:- mode(set_attributes(+list), one).
	:- info(set_attributes/1,
		[comment is 'Sets a list of attribute-value pairs.',
		 argnames is ['Attributes']]).

	attribute(Attribute, Value) :-
		::attribute_(Attribute, Value).

	attributes(Attributes) :-
		findall(Attribute, ::attribute_(Attribute, _), Attributes).

	del_attribute(Attribute, Value) :-
		::retract(attribute_(Attribute, Value)).

	del_attributes(Attribute, Value) :-
		::retractall(attribute_(Attribute, Value)).

	set_attribute(Attribute, Value) :-
		::retractall(attribute_(Attribute, _)),
		::assertz(attribute_(Attribute, Value)).

	set_attributes([]).
	set_attributes([Attribute-Value| Attributes]) :-
		::retractall(attribute_(Attribute, _)),
		::assertz(attribute_(Attribute, Value)),
		set_attributes(Attributes).

:- end_category.
