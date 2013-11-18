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
		date is 2012/11/28,
		comment is 'Unit tests for the dynamic/0 built-in directive.'
	]).

	:- dynamic.

	test(dynamic_0_1) :-
		this(This),
		object_property(This, (dynamic)).

	test(dynamic_0_2) :-
		this(This),
		\+ object_property(This, static).

	% all predicates in a dynamic entity are implicitly dynamic

	:- private(p/0).

	test(dynamic_0_3) :-
		predicate_property(p, private),
		predicate_property(p, (dynamic)).

:- end_object.
