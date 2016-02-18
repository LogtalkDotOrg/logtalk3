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


/*
In order to better grasp this example, draw a diagram of the hierarchy made of
the three objects below with their instantiation and specialization relations.
*/


:- object(object,			% root of the inheritance graph
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

	strict_instance.		% descendant instances of this class
							% are, by default, strict instances
	print :-
		self(Self),
		write('Object: '), writeq(Self), nl, nl,
		write('  interface:'), nl,
		forall(
			::current_predicate(Predicate),
			(write('    '), writeq(Predicate), nl)),
		nl.

:- end_object.


:- object(class,			% default metaclass for all instantiable classes
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

	abstract_class :-		% instances of this class are instantiable classes,
		fail.				% not abstract classes

:- end_object.


:- object(abstract_class,	% default metaclass for all abstract classes
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

	abstract_class :-		% by default, descendant instances of this class are abstract
		self(Self),			% classes except this class itself which is an instantiable class
		Self \= abstract_class.

	metaclass :-			% descendant instances of this class are metaclasses if
		self(Self),			% their instances are themselves classes, i.e. if their 
		once((				% instances accept the abstract_class/0 message 
			instantiates_class(Class, Self),
			Class::current_predicate(abstract_class/0))).

	strict_instance :-		% instances of this class are not strict instances;
		fail.				% they are classes

:- end_object.
