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
		date is 2014/05/07,
		comment is 'Unit tests for the dynamic/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).
	:- dynamic(a/0).

	:- private((b/1, c/2)).
	:- dynamic((b/1, c/2)).

	:- private([d/3, e/4]).
	:- dynamic([d/3, e/4]).

	test(dynamic_1_1) :-
		predicate_property(a, (dynamic)).

	test(dynamic_1_2) :-
		\+ predicate_property(a, static).

	test(dynamic_1_3) :-
		predicate_property(b(_), (dynamic)),
		predicate_property(c(_,_), (dynamic)).

	test(dynamic_1_4) :-
		\+ predicate_property(b(_), static),
		\+ predicate_property(c(_,_), static).

	test(dynamic_1_5) :-
		predicate_property(d(_,_,_), (dynamic)),
		predicate_property(e(_,_,_,_), (dynamic)).

	test(dynamic_1_6) :-
		\+ predicate_property(d(_,_,_), static),
		\+ predicate_property(e(_,_,_,_), static).

	% local (dynamic) predicates, i.e. predicates without
	% a scope directive, are invisible to the reflection
	% built-in predicates such as predicate_property/2

	:- dynamic(g/6).

	test(dynamic_1_7) :-
		\+ predicate_property(g(_,_,_,_,_,_), _).

:- end_object.
