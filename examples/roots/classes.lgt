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


:- protocol(abstract_classp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2010/11/20,
		comment is 'Default protocol for all abstract classes.'
	]).

	:- public(metaclass/0).
	:- mode(metaclass, zero_or_one).
	:- info(metaclass/0, [
		comment is 'True if the object is a metaclass.'
	]).

	:- public(abstract_class/0).
	:- mode(abstract_class, zero_or_one).
	:- info(metaclass/0, [
		comment is 'True if the object is an abstract class.'
	]).

:- end_protocol.



:- object(abstract_class,
	implements(abstract_classp),
	instantiates(class),
	specializes(object)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default metaclass for all abstract classes.'
	]).

	metaclass :-
		self(Self),
		instantiates_class(Class, Self),
		this(This),
		Class::ancestor(This).

	abstract_class :-
		self(Self),
		Self \= abstract_class.

	strict_instance :-
		fail.

:- end_object.



:- protocol(classp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default protocol for all instantiable classes.'
	]).

	:- public(new/1).
	:- mode(new(?object), zero_or_one).
	:- info(new/1, [
		comment is 'Creates a new instance.',
		argnames is ['Instance']
	]).

	:- public(new/2).
	:- mode(new(?object, +list), zero_or_one).
	:- info(new/2, [
		comment is 'Creates a new instance using a list of initialization options.',
		argnames is ['Instance', 'Options']
	]).

	:- public(clone/2).
	:- mode(clone(+object, ?object), zero_or_one).
	:- info(clone/2, [
		comment is 'Clones an instance.',
		argnames is ['Instance', 'Clone']
	]).

	:- public(instance_base_name/1).
	:- mode(instance_base_name(-atom), one).
	:- info(instance_base_name/1, [
		comment is 'Base name to generated new instance names.',
		argnames is ['Name']
	]).

	:- public(delete/1).
	:- mode(delete(+object), zero_or_one).
	:- info(delete/1, [
		comment is 'Deletes a dynamic instance.',
		argnames is ['Instance']
	]).

	:- public(delete/2).
	:- mode(delete(+object, +list), zero_or_one).
	:- info(delete/2, [
		comment is 'Deletes a dynamic instance using a list of deleting options.',
		argnames is ['Instance', 'Options']
	]).

	:- public(delete_all/0).
	:- mode(delete_all, zero_or_one).
	:- info(delete_all/0, [
		comment is 'Deletes all dynamic instances. Fails if some dynamic instance can not be deleted.'
	]).

	:- public(delete_all/1).
	:- mode(delete_all(+list), zero_or_one).
	:- info(delete_all/1, [
		comment is 'Deletes all dynamic instances using a list of deleting options. Fails if some dynamic instance can not be deleted.',
		argnames is ['Options']
	]).

	:- public(equals/2).
	:- mode(equals(+object, +object), zero_or_one).
	:- info(equals/2, [
		comment is 'The two instances represents the same object for some definition of equality.',
		argnames is ['Instance1', 'Instance2']
	]).

:- end_protocol.



:- object(class,
	implements(classp),
	instantiates(class),
	specializes(abstract_class)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/3/12,
		comment is 'Default metaclass for all classes.'
	]).

	:- private(instance_counter_/1).
	:- dynamic(instance_counter_/1).
	:- mode(instance_counter_(?integer), zero_or_one).
	:- info(instance_counter_/1, [
		comment is 'Stores a counter of created instances.',
		argnames is ['Counter']
	]).

	new(Object) :-
		::new(Object, []).

	new(Object, Options) :-
		valid_new_identifier(Object),
		self(Self),
		create_object(Object, [instantiates(Self)], [], []),
		Object::init(Options).

	clone(Object, Clone) :-
		self(Self),
		sender(Sender),
		throw(error(subclass_responsability, Self::clone(Object, Clone), Sender)).

	delete(Object) :-
		::delete(Object, []).

	delete(Object, Options) :-
		::instance(Object),
		Object::free(Options),
		abolish_object(Object).

	delete_all :-
		::delete_all([]).

	delete_all(Options) :-
		::instance(Instance),
		(	object_property(Instance, (dynamic)) ->
			::delete(Instance, Options)
		;	true
		),
		fail.

	delete_all(_) :-
		\+ (::instance(Instance),
			object_property(Instance, (dynamic))).

	instance_base_name(i).

	instance_counter_(1).

	valid_new_identifier(Identifier) :-
		var(Identifier), !,
		retract(instance_counter_(Last)),
		::instance_base_name(Base),
		repeat,
			next_integer(Last, Next),
			number_codes(Next, Codes),
			atom_codes(Atom, Codes),
			atom_concat(Base, Atom, Identifier),
		\+ current_object(Identifier),
		\+ current_category(Identifier),
		\+ current_protocol(Identifier),
		asserta(instance_counter_(Next)),
		!.
	valid_new_identifier(Identifier) :-
		once((atom(Identifier); compound(Identifier))),
		\+ current_object(Identifier),
		\+ current_category(Identifier),
		\+ current_protocol(Identifier).

	next_integer(N, N).
	next_integer(N, N2) :-
		N1 is N + 1,
		next_integer(N1, N2).

	equals(Instance1, Instance2) :-
		self(Self),
		sender(Sender),
		throw(error(subclass_responsability, Self::equals(Instance1, Instance2), Sender)).

	abstract_class :-
		fail.

:- end_object.



:- protocol(objectp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default protocol for all objects.'
	]).

	:- public(strict_instance/0).
	:- mode(strict_instance, zero_or_one).
	:- info(strict_instance/0, [
		comment is 'True if the object is strictly an instance.'
	]).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0, [
		comment is 'Pretty prints an object description.'
	]).

	:- public(nil/0).
	:- mode(nil, zero_or_one).
	:- info(nil/0, [
		comment is 'True if the object represents a void reference.'
	]).

:- end_protocol.



:- object(object,
	implements((objectp, monitoring)),
	imports(((initialization), class_hierarchy)),
	instantiates(class)).

	:- info([
		version is 1.2,
		date is 2013/04/23,
		author is 'Paulo Moura',
		comment is 'Minimal predicates for all objects. Default root of the inheritance graph.'
	]).

	:- uses(event_registry, [
		del_monitors/4
	]).

	strict_instance.

	default_free_option(del_monitors).

	process_free_option(del_monitors) :-
		self(Self),
		del_monitors(Self, _, _, _),
		del_monitors(_, _, Self, _),
		del_monitors(_, _, _, Self).

	nil :-
		fail.

	print :-
		self(Self),
		writeq(Self), nl, nl,
		forall(
			::current_predicate(Predicate),
			(writeq(Predicate), nl)),
		nl.

	before(_, _, _).

	after(_, _, _).

:- end_object.
