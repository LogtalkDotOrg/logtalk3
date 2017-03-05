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


/*
In order to better grasp this example, draw a diagram of the hierarchy made of
the three objects below with their instantiation and specialization relations.
*/


% root of the inheritance graph
:- object(object,
	instantiates(class)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Inheritance root for all objects.'
	]).

	:- public(strict_instance/0).
	:- mode(strict_instance, zero_or_one).

	:- public(print/0).
	:- mode(print, one).

	% descendant instances of this class
	% are, by default, strict instances
	strict_instance.

	print :-
		self(Self),
		write('Object: '), writeq(Self), nl, nl,
		write('  interface:'), nl,
		forall(
			::current_predicate(Predicate),
			(write('    '), writeq(Predicate), nl)),
		nl.

:- end_object.


% default metaclass for all instantiable classes
:- object(class,
	instantiates(class),
	specializes(abstract_class)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Instantiation root and default metaclass for all classes.'
	]).

	:- public(new/1).
	:- mode(new(+object), zero_or_one).

	:- public(delete/1).
	:- mode(delete(+object), zero_or_one).

	:- public(instances/1).
	:- mode(instances(-list), one).

	new(Object) :-
		self(Self),
		create_object(Object, [instantiates(Self)], [], []).

	delete(Object) :-
		self(Self),
		instantiates_class(Object, Self),
		\+ instantiates_class(_, Object),
		\+ specializes_class(_, Object),
		abolish_object(Object).

	instances(Instances) :-
		self(Self),
		findall(Instance, instantiates_class(Instance, Self), Instances).

	% instances of this class are instantiable
	% classes, not abstract classes
	abstract_class :-
		fail.

:- end_object.


% default metaclass for all abstract classes
:- object(abstract_class,
	instantiates(class),
	specializes(object)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Default metaclass for all abstract classes.'
	]).

	:- public(metaclass/0).
	:- mode(metaclass, zero_or_one).

	:- public(abstract_class/0).
	:- mode(abstract_class, zero_or_one).

	% by default, descendant instances of this class are abstract
	% classes except this class itself which is an instantiable class
	abstract_class :-
		self(Self),
		Self \= abstract_class.

	% descendant instances of this class are metaclasses if
	% their instances are themselves classes, i.e. if their
	% instances accept the abstract_class/0 message
	metaclass :-
		self(Self),
		once((
			instantiates_class(Class, Self),
			Class::current_predicate(abstract_class/0)
		)).

	% instances of this class are not strict
	% instances; they are classes
	strict_instance :-
		fail.

:- end_object.
