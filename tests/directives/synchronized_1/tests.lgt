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
		date is 2013/02/28,
		comment is 'Unit tests for the synchronized/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).
	:- synchronized(a/0).

	:- private([b/1, c/2]).
	:- synchronized([b/1, c/2]).

	:- if(current_logtalk_flag(threads, supported)).

		test(synchronized_1_1) :-
			predicate_property(a, private),
			predicate_property(a, synchronized).

		test(synchronized_1_2) :-
			predicate_property(b(_), private),
			predicate_property(b(_), synchronized).

		test(synchronized_1_3) :-
			predicate_property(c(_,_), private),
			predicate_property(c(_,_), synchronized).

	:- else.

		% when threads are not supported, the synchronized/1 directive is ignored

		test(synchronized_1_1) :-
			predicate_property(a, private),
			\+ predicate_property(a, synchronized).

		test(synchronized_1_2) :-
			predicate_property(b(_), private),
			\+ predicate_property(b(_), synchronized).

		test(synchronized_1_3) :-
			predicate_property(c(_,_), private),
			\+ predicate_property(c(_,_), synchronized).

	:- endif.

:- end_object.
