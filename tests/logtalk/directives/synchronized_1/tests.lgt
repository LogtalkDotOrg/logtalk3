%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/16,
		comment is 'Unit tests for the synchronized/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).
	:- synchronized(a/0).

	:- private((b/1, c/2)).
	:- synchronized((b/1, c/2)).

	:- private([d/3, e/4]).
	:- synchronized([d/3, e/4]).

	% test synchronized predicate calls

	:- public(s/1).
	:- synchronized(s/1).

	s(1).
	s(2).
	s(3).

	:- public(t/1).

	t(X) :-
		s(X).

	% calls to predicates declared synchronized but not defined
	% must fail instead of throwing an existence error

	:- synchronized(r/2).

	test(synchronized_1_1) :-
		predicate_property(a, private),
		predicate_property(a, synchronized).

	test(synchronized_1_2) :-
		predicate_property(b(_), private),
		predicate_property(b(_), synchronized),
		predicate_property(c(_,_), private),
		predicate_property(c(_,_), synchronized).

	test(synchronized_1_3) :-
		predicate_property(d(_,_,_), private),
		predicate_property(d(_,_,_), synchronized),
		predicate_property(e(_,_,_,_), private),
		predicate_property(e(_,_,_,_), synchronized).

		% when threads are not supported, the synchronized/1 directive simply makes
		% the predicates deterministic by wrapping its calls using once/1

	test(synchronized_1_4) :-
		findall(X, s(X), L),
		L == [1].

	test(synchronized_1_5) :-
		findall(X, t(X), L),
		L == [1].

	test(synchronized_1_6) :-
		\+ r(_, _).

:- end_object.
