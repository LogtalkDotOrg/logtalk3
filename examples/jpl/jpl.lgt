%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
	implements(forwarding, javap)).

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
