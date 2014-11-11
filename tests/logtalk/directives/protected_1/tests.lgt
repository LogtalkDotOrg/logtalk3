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
		comment is 'Unit tests for the protected/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- protected(a/0).
	:- protected((b/1, c/2)).
	:- protected([d/3, e/4]).

	test(protected_1_1) :-
		predicate_property(a, protected),
		predicate_property(a, static).

	test(protected_1_2) :-
		predicate_property(b(_), protected),
		predicate_property(b(_), static),
		predicate_property(c(_,_), protected),
		predicate_property(c(_,_), static).

	test(protected_1_3) :-
		predicate_property(d(_,_,_), protected),
		predicate_property(d(_,_,_), static),
		predicate_property(e(_,_,_,_), protected),
		predicate_property(e(_,_,_,_), static).

:- end_object.
