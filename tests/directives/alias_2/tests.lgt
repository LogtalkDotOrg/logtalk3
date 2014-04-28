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
		date is 2014/04/28,
		comment is 'Unit tests for the alias/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- alias(lgtunit, [run/0 as run_alias1/0]).
	:- alias(lgtunit, [run/0 :: run_alias2/0]).

	test(alias_2_1) :-
		predicate_property(run_alias1, alias_of(run)),
		predicate_property(run_alias1, declared_in(lgtunit)),
		predicate_property(run_alias1, defined_in(lgtunit)).

	test(alias_2_2) :-
		predicate_property(run_alias2, alias_of(run)),
		predicate_property(run_alias2, declared_in(lgtunit)),
		predicate_property(run_alias2, defined_in(lgtunit)).

:- end_object.
