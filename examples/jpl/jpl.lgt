%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(javap).

	:- info([
		version is 1.0,
		author is 'Paulo Moura and Sergio Castro',
		date is 2014/03/27,
		comment is 'Protocol for a minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.'
	]).

	:- public(get_field/2).
	:- mode(get_field(+atom, ?nonvar), zero_or_one).
	:- info(get_field/2, [
		comment is 'Gets the value of a class field.',
		argnames is ['Field', 'Value']
	]).

	:- public(set_field/2).
	:- mode(set_field(+atom, +nonvar), one).
	:- info(set_field/2, [
		comment is 'Sets the value of a class field.',
		argnames is ['Field', 'Value']
	]).

	:- public(new/2).
	:- mode(new(+list(nonvar), -reference), one).
	:- info(new/2, [
		comment is 'Creates a new instance using the specified parameter values.',
		argnames is ['Parameters', 'Instance']
	]).

	:- public(new/1).
	:- mode(new(-reference), one).
	:- info(new/1, [
		comment is 'Creates a new instance using default parameter values.',
		argnames is ['Instance']
	]).

	:- public(invoke/1).
	:- mode(invoke(@nonvar), one).
	:- info(invoke/1, [
		comment is 'Invokes a method. This is more efficient than relying on the forward/1 handler.',
		argnames is ['Message']
	]).

:- end_protocol.


:- object(java(_Reference, _ReturnValue),
	implements((forwarding, javap))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura and Sergio Castro',
		date is 2014/03/27,
		comment is 'Minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.',
		parnames is ['Reference', 'ReturnValue']
	]).

	:- use_module(jpl, [
		jpl_get/3, jpl_set/3,
		jpl_new/3,
		jpl_call/4
	]).

	get_field(Field, Value) :-
		parameter(1, Class),
		jpl_get(Class, Field, Value).

	set_field(Field, Value) :-
		parameter(1, Class),
		jpl_set(Class, Field, Value).

	new(Parameters, Instance) :- 
		parameter(1, Class),
		jpl_new(Class, Parameters, Instance),
		parameter(2, Instance).

	new(Instance) :- 
		new([], Instance).

	invoke(Message) :-
		parameter(1, Reference),
		Message =.. [Functor| Arguments],
		jpl_call(Reference, Functor, Arguments, Output),
		parameter(2, Output).

	forward(Message) :-
		parameter(1, Reference),
		Message =.. [Functor| Arguments],
		jpl_call(Reference, Functor, Arguments, Output),
		parameter(2, Output).

:- end_object.


:- object(java(Reference),
	extends(java(Reference, _))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura and Sergio Castro',
		date is 2014/03/25,
		comment is 'Minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.',
		parnames is ['Reference']
	]).

:- end_object.


:- object(java).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2016/05/19,
		comment is 'Abstract interface to JPL API utility predicates.'
	]).

	:- public(array_list/2).
	:- mode(array_list(+array, -list), one).
	:- mode(array_list(-array, +list), one).
	:- info(array_list/2, [
		comment is 'Converts between an array and a list.',
		argnames is ['Array', 'List']
	]).

	:- public(null/1).
	:- mode(null(@nonvar), zero_or_one).
	:- info(null/1, [
		comment is 'True when the argument is Java null',
		argnames is ['Reference']
	]).

	:- public(iterator_element/2).
	:- mode(iterator_element(+iterator, -element), zero_or_more).
	:- info(iterator_element/2, [
		comment is 'Enumerates, by backtracking, all iterator elements.',
		argnames is ['Array', 'List']
	]).

	:- use_module(jpl, [
		jpl_list_to_array/2, jpl_array_to_list/2,
		jpl_is_null/1,
		jpl_iterator_element/2
	]).

	array_list(Array, List) :-
		(	var(Array) ->
			jpl_list_to_array(List, Array)
		;	jpl_array_to_list(Array, List)
		).

	null(Reference) :-
		jpl_is_null(Reference).

	iterator_element(Iterator,Element) :-
		jpl_iterator_element(Iterator, Element).

:- end_object.
