%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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



:- category(attributes).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Dynamic attributes dictionary.'
	]).

	:- public(attribute/2).
	:- mode(attribute(?nonvar, ?nonvar), zero_or_more).
	:- info(attribute/2, [
		comment is 'Returns, by backtracking, all pairs of attribute-values.',
		argnames is ['Attribute', 'Value']
	]).

	:- public(attributes/1).
	:- mode(attributes(-list), one).
	:- info(attributes/1, [
		comment is 'List of all pairs of attribute-values.',
		argnames is ['Attributes']
	]).

	:- private(attribute_/2).
	:- dynamic(attribute_/2).
	:- mode(attribute_(?nonvar, ?nonvar), zero_or_more).
	:- info(attribute_/2, [
		comment is 'Stores attributes values.',
		argnames is ['Attribute', 'Value']
	]).

	:- public(del_attribute/2).
	:- mode(del_attribute(?nonvar, ?nonvar), zero_or_more).
	:- info(del_attribute/2, [
		comment is 'Deletes a matching attribute-value pair.',
		argnames is ['Attribute', 'Value']
	]).

	:- public(del_attributes/2).
	:- mode(del_attributes(@term, @term), one).
	:- info(del_attributes/2, [
		comment is 'Deletes all matching attribute-value pairs.',
		argnames is ['Attribute', 'Value']
	]).

	:- public(set_attribute/2).
	:- mode(set_attribute(+nonvar, +nonvar), one).
	:- info(set_attribute/2, [
		comment is 'Sets an attribute value.',
		argnames is ['Attribute', 'Value']
	]).

	:- public(set_attributes/1).
	:- mode(set_attributes(+list), one).
	:- info(set_attributes/1, [
		comment is 'Sets a list of attribute-value pairs.',
		argnames is ['Attributes']
	]).

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
