%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
This file contains an adaptation to Logtalk of code for logical assignment 
of Prolog terms developed by Nobukuni Kino. For more information, please 
consult the URL http://www.kprolog.com/en/logical_assignment/

As a derivative work, this file is licensed under the Open Software License 
version 2.1 (http://opensource.org/licenses/osl-2.1.php).
*/


:- op(100, xfx, '<=').
:- op(100, xfx, '=>').


:- object(assignvars,
	implements(assignvarsp)).

	:- info([
		version is 1.3,
		author is 'Nobukuni Kino and Paulo Moura',
		date is 2015/07/31,
		comment is 'Assignable variables (supporting backtracable assignement of non-variable terms).'
	]).

	assignable(Assignable) :-
		nonvar(Assignable),
		self(Self),
		sender(Sender),
		throw(error(type_error(variable, Assignable), logtalk(Self::assignable(Assignable), Sender))).
	assignable([_| _]).

	assignable(Assignable, Init) :-
		nonvar(Assignable),
		self(Self),
		sender(Sender),
		throw(error(type_error(variable, Assignable), logtalk(Self::assignable(Assignable, Init), Sender))).
	assignable(Assignable, Init) :-
		var(Init),
		self(Self),
		sender(Sender),
		throw(error(instantiation_error, logtalk(Self::assignable(Assignable, Init), Sender))).
	assignable([_, Init| _], Init).

	Assignable <= Value :-
		var(Value),
		self(Self),
		sender(Sender),
		throw(error(instantiation_error, logtalk(Self::Assignable <= Value, Sender))).

	[_| Tail] <= Value :-
		put_assign([_| Tail], Value).

	put_assign([_| Tail], Value) :-
		(	nonvar(Tail) ->
			put_assign(Tail, Value)
		;	Tail = [Value| _]
		).

	Assignable => Value :-
		var(Assignable),
		self(Self),
		sender(Sender),
		throw(error(instantiation_error, logtalk(Self::Assignable => Value, Sender))).

	[_| Tail] => Value :-
		nonvar(Tail),
		peek_assign(Tail, Value).

	peek_assign([Current| Tail], Value) :-
		(	nonvar(Tail) ->
			peek_assign(Tail, Value)
		;	Current = Value
		).

:- end_object.
