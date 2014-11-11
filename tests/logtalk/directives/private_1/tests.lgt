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
		comment is 'Unit tests for the private/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- private(a/0).
	:- private((b/1, c/2)).
	:- private([d/3, e/4]).

	test(private_1_1) :-
		predicate_property(a, private),
		predicate_property(a, static).

	test(private_1_2) :-
		predicate_property(b(_), private),
		predicate_property(b(_), static),
		predicate_property(c(_,_), private),
		predicate_property(c(_,_), static).

	test(private_1_3) :-
		predicate_property(d(_,_,_), private),
		predicate_property(d(_,_,_), static),
		predicate_property(e(_,_,_,_), private),
		predicate_property(e(_,_,_,_), static).

:- end_object.
