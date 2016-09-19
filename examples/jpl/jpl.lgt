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
		version is 1.01,
		author is 'Paulo Moura and Sergio Castro',
		date is 2016/09/19,
		comment is 'Minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.',
		parameters is [
			'Reference' - 'Either a class name or a Java reference to an object',
			'ReturnValue' - 'Value returned by a method call (possibly the Java value void)'
		]
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
		parameters is [
			'Reference' - 'Either a class name or a Java reference to an object'
		]
	]).

:- end_object.


:- object(java).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/09/19,
		comment is 'Abstract interface to JPL API utility predicates.'
	]).

	:- public(true/1).
	:- mode(true(--var), one).
	:- info(true/1, [
		comment is 'Returns an opaque term that represents the Java value true.',
		argnames is ['Reference']
	]).

	:- public(false/1).
	:- mode(false(--var), one).
	:- info(false/1, [
		comment is 'Returns an opaque term that represents the Java value false.',
		argnames is ['Reference']
	]).

	:- public(void/1).
	:- mode(void(--var), one).
	:- info(void/1, [
		comment is 'Returns an opaque term that represents the Java value void.',
		argnames is ['Reference']
	]).

	:- public(null/1).
	:- mode(null(--var), one).
	:- info(null/1, [
		comment is 'Returns an opaque term that represents the Java value null.',
		argnames is ['Reference']
	]).

	:- public(is_true/1).
	:- mode(is_true(++ground), zero_or_one).
	:- info(is_true/1, [
		comment is 'True when the argument is the Java value true. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_false/1).
	:- mode(is_false(++ground), zero_or_one).
	:- info(is_false/1, [
		comment is 'True when the argument is the Java value false. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_void/1).
	:- mode(is_void(++ground), zero_or_one).
	:- info(is_void/1, [
		comment is 'True when the argument is the Java value void. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_null/1).
	:- mode(is_null(++ground), zero_or_one).
	:- info(is_null/1, [
		comment is 'True when the argument is the Java value null. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(array_list/2).
	:- mode(array_list(+array, -list), one).
	:- mode(array_list(-array, +list), one).
	:- info(array_list/2, [
		comment is 'Converts between an array and a list.',
		argnames is ['Array', 'List']
	]).

	:- public(iterator_element/2).
	:- mode(iterator_element(+iterator, -element), zero_or_more).
	:- info(iterator_element/2, [
		comment is 'Enumerates, by backtracking, all iterator elements.',
		argnames is ['Array', 'List']
	]).

	:- use_module(jpl, [
		jpl_true/1, jpl_false/1, jpl_void/1, jpl_null/1,
		jpl_is_true/1, jpl_is_false/1, jpl_is_void/1, jpl_is_null/1,
		jpl_list_to_array/2, jpl_array_to_list/2,
		jpl_iterator_element/2
	]).

	true(Reference) :-
		jpl_true(Reference).

	false(Reference) :-
		jpl_false(Reference).

	void(Reference) :-
		jpl_void(Reference).

	null(Reference) :-
		jpl_null(Reference).

	is_true(Reference) :-
		jpl_is_true(Reference).

	is_false(Reference) :-
		jpl_is_false(Reference).

	is_void(Reference) :-
		jpl_is_void(Reference).

	is_null(Reference) :-
		jpl_is_null(Reference).

	array_list(Array, List) :-
		(	var(Array) ->
			jpl_list_to_array(List, Array)
		;	jpl_array_to_list(Array, List)
		).

	iterator_element(Iterator,Element) :-
		jpl_iterator_element(Iterator, Element).

:- end_object.
