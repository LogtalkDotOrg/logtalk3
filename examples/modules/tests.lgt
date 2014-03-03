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
		version is 1.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/04,
		comment is 'Unit tests for the "modules" example.'
	]).

	cover(exports).
	cover(test).
	cover(metapreds).
	:- if(current_object(client)).	% client for testing use_module/1 directives, which are
		cover(client).				% only supported for some back-end Prolog compilers
	:- endif.

	test(modules_1) :-
		findall(Predicate, exports::current_predicate(Predicate), Predicates),
		Predicates == [p/1].

	test(modules_2) :-
		setof(Property, Predicate^(exports::predicate_property(p(Predicate), Property)), AllProperties),
		list::msort([logtalk, public, static, declared_in(exports), defined_in(exports), scope(public)], Properties),
		list::subsequence(AllProperties, Properties, _).

	test(modules_3) :-
		findall(N, exports::p(N), Solutions),
		Solutions == [1, 2, 3].

	test(modules_4) :-
		test::names(Names),
		Names == [paulo, carlos, helena].

	test(modules_5) :-
		test::test(Names),
		Names == [paulo, carlos, helena].

	:- if(current_object(client)).	% client for testing use_module/1 directives, which are
									% only supported for some back-end Prolog compilers
		test(modules_6) :-
			client::names(Names),
			Names == [paulo, carlos, helena].

		test(modules_7) :-
			client::test(Names),
			Names == [paulo, carlos, helena].

	:- endif.

:- end_object.
