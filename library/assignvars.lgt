
/*
This file contains an adaptation to Logtalk of code for logical assignment 
of Prolog terms developed by Nobukuni Kino. For more information, please 
consult the URL http://www.kprolog.com/en/logical_assignment/

As a derivative work, this file is licensed under the Open Software License 
version 2.1 (http://opensource.org/licenses/osl-2.1.php).
*/


:- op(100, xfx, '<=').
:- op(100, xfx, '=>').


:- category(assignvars).

	:- info([
		version is 1.0,
		author is 'Nobukuni Kino and Paulo Moura',
		date is 2005/1/7,
		comment is 'Assignable variables (supporting logical, backtracable assignement of non-variable terms).']).

	:- public(assignable/1).
	:- mode(assignable(-assignvar), one).
	:- info(assignable/1, [
		comment is 'Makes Variable an assignable variable. Initial state will be empty.',
		argnames is ['Variable'],
		exceptions is [
			'Variable is not a variable' - type_error(variable, 'Variable')]]).

	:- public(assignable/2).
	:- mode(assignable(-assignvar, @nonvar), one).
	:- info(assignable/2, [
		comment is 'Makes Variable an assignable variable and sets its initial state to Value.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not a variable' - type_error(variable, 'Variable'),
			'Value is not instantiated' - instantiation_error]]).

	:- public((<=)/2).
	:- mode(<=(?assignvar, @nonvar), one).
	:- info((<=)/2, [
		comment is 'Sets the state of the assignable variable Variable to Value (initializing the variable if needed).',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Value is not instantiated' - instantiation_error]]).

	:- public((=>)/2).
	:- mode(=>(+assignvar, ?nonvar), zero_or_one).
	:- info((=>)/2, [
		comment is 'Unifies Value with the current state of the assignable variable Variable.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not instantiated' - instantiation_error]]).

	:- op(100, xfx, <=).
	:- op(100, xfx, =>).

	assignable(Assig) :-
		nonvar(Assig),
		self(Self),
		sender(Sender),
		throw(error(type_error(variable, Assig), Self::assignable(Assig), Sender)).
	assignable([_| _]).

	assignable(Assig, Init) :-
		nonvar(Assig),
		self(Self),
		sender(Sender),
		throw(error(type_error(variable, Assig), Self::assignable(Assig, Init), Sender)).
	assignable(Assig, Init) :-
		var(Init),
		self(Self),
		sender(Sender),
		throw(error(instantiation_error, Self::assignable(Assig, Init), Sender)).
	assignable([_, Init| _], Init).

	Assig <= Value :-
		var(Value),
		self(Self),
		sender(Sender),
		throw(error(instantiation_error, Self::Assig <= Value, Sender)).

	[_| Tail] <= Value :-
		(	nonvar(Tail) ->
			Tail <= Value
		;	Tail = [Value| _]
		).

	Assig => Value :-
		var(Assig),
		self(Self),
		sender(Sender),
		throw(error(instantiation_error, Self::Assig => Value, Sender)).

	[Current| Tail] => Value :-
		(	nonvar(Tail) ->
			Tail => Value
		;	Current = Value
		).

:- end_category.
