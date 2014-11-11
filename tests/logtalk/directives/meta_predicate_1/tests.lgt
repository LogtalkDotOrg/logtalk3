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
		date is 2013/11/18,
		comment is 'Unit tests for the meta_predicate/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(foo/1).
	:- meta_predicate(foo(0)).

	test(meta_predicate_1_1) :-
		predicate_property(foo(_), meta_predicate(Template)),
		Template == foo(0).

	:- public(bar/2).
	:- meta_predicate(bar(^, *)).

	test(meta_predicate_1_2) :-
		predicate_property(bar(_,_), meta_predicate(Template)),
		Template == bar(^, *).

	:- public(baz/3).
	:- meta_predicate(baz(2, *, *)).

	test(meta_predicate_1_3) :-
		predicate_property(baz(_,_,_), meta_predicate(Template)),
		Template == baz(2, *, *).

	:- public(qux/2).
	:- meta_predicate(qux(::, *)).

	test(meta_predicate_1_4) :-
		predicate_property(qux(_,_), meta_predicate(Template)),
		Template == qux(::, *).

:- end_object.
