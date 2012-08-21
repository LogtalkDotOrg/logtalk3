
/*
This file contains an adaptation to Logtalk of code for logical assignment 
of Prolog terms developed by Nobukuni Kino. For more information, please 
consult the URL http://www.kprolog.com/en/logical_assignment/

As a derivative work, this file is licensed under the Open Software License 
version 2.1 (http://opensource.org/licenses/osl-2.1.php).
*/

:- object(streamvars).

	:- info([
		version is 1.0,
		author is 'Nobukuni Kino and Paulo Moura',
		date is 2011/08/17,
		comment is 'Stream variables (supporting logical, backtracable, adding and retrieving of terms).']).

	:- public(new/1).
	:- mode(new(-streamvar), one).
	:- info(new/1, [
		comment is 'Makes Variable a stream variable. Initial state will be empty.',
		argnames is ['Variable'],
		exceptions is [
			'Variable is not a variable' - type_error(variable, 'Variable')]]).

	:- public(new/2).
	:- mode(new(-streamvar, @nonvar), one).
	:- info(new/2, [
		comment is 'Makes Variable a stream variable and sets its initial state to Value.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not a variable' - type_error(variable, 'Variable')]]).

	:- public((<=)/2).
	:- mode(<=(?streamvar, @nonvar), one).
	:- info((<=)/2, [
		comment is 'Sets the state of the stream variable Variable to Value (initializing the variable if needed).',
		argnames is ['Variable', 'Value']]).

	:- public(op(100, xfx, <=)).

	:- public((=>)/2).
	:- mode(=>(+streamvar, ?nonvar), zero_or_one).
	:- info((=>)/2, [
		comment is 'Unifies Value with the current state of the stream variable Variable.',
		argnames is ['Variable', 'Value']]).

	:- public(op(100, xfx, =>)).

	new(StreamVar) :-
		nonvar(StreamVar),
		this(This),
		sender(Sender),
		throw(error(type_error(variable, StreamVar), logtalk(This::new(StreamVar), Sender))).
	new([_| _]).

	new(StreamVar, Init) :-
		nonvar(StreamVar),
		this(This),
		sender(Sender),
		throw(error(type_error(variable, StreamVar), logtalk(This::new(StreamVar, Init), Sender))).
	new([_, v(Init)| _], Init).

	[_| Tail] <= Value :-
		(	nonvar(Tail) ->
			Tail <= Value
		;	Tail = [v(Value)| _]
		).

	[v(Current)| Tail] => Value :-
		(	nonvar(Tail) ->
			Tail => Value
		;	Current = Value
		).

:- end_object.
