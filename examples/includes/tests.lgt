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
		date is 2014/05/21,
		comment is 'Unit tests for the "includes" example.'
	]).

	test(includes_1) :-
		{findall(Vowel, vowel(Vowel), Vowels)},
		Vowels == [a,e,i,o,u].

	test(includes_2) :-
		countries::capitals(Capitals),
		Capitals == [berlim, lisbon, madrid, paris, varsovia].

	test(includes_3) :-
		setof(Countries, countries::same_population(Countries), Solutions),
		Solutions == [[france, polony], [germany, spain], [portugal]].

	test(includes_4) :-
		create_object(capitals, [], [public(capital/1), include(includes('countries.pl'))], [(capital(Capital) :- country(_,Capital,_))]),
		{setof(Capital, capitals::capital(Capital), Capitals)},
		Capitals == [berlim, lisbon, madrid, paris, varsovia],
		abolish_object(capitals).

:- end_object.
