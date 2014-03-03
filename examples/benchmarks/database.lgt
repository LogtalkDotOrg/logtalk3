%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(database).

	:- info([
		version is 3.2,
		author is 'Paulo Moura',
		date is 2014/01/16,
		comment is 'Dynamic database benchmark utility predicates.'
	]).

	:- public(this_dyndb/1).
	:- mode(this_dyndb(+nonvar), one).
	:- info(this_dyndb/1, [
		comment is 'Asserts and retracts a fact in "this".',
		argnames is ['Term']
	]).

	:- public(self_dyndb/1).
	:- mode(self_dyndb(+nonvar), one).
	:- info(self_dyndb/1, [
		comment is 'Asserts and retracts a fact using ::/1.',
		argnames is ['Term']
	]).

	:- public(other_dyndb/1).
	:- mode(other_dyndb(+nonvar), one).
	:- info(other_dyndb/1, [
		comment is 'Asserts and retracts a fact using ::/2.',
		argnames is ['Term']
	]).

	:- private([pred_this/4, pred_self/4]).
	:- dynamic([pred_this/4, pred_self/4]).

	% direct calls to assertz/1 and retract/1:
	this_dyndb(N) :-
		retractall(pred_this(N, _, _, _)),
		assertz(pred_this(N, _, a, 3.14)).

	% calls to assertz/1 and retract/1 using ::/1:
	self_dyndb(N) :-
		::retractall(pred_self(N, _, _, _)),
		::assertz(pred_self(N, _, a, 3.14)).

	% calls to assertz/1 and retract/1 using ::/2:
	other_dyndb(N) :-
		database_other::retractall(pred_other(N, _, _, _)),
		database_other::assertz(pred_other(N, _, a, 3.14)).

:- end_object.



:- object(database_self,
	extends(database)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/01/16,
		comment is 'Test object for the database predicate benchmarks.'
	]).

:- end_object.
