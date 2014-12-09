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
		date is 2014/12/08,
		comment is 'Unit tests for the category/3 opening directive.'
	]).

	% test all possible syntaxes for category relations

	test(category_0) :-
		logtalk_load(categories, [unknown_entities(silent)]).

	test(category_1) :-
		implements_protocol(category_1, protocol1),
		extends_category(category_1, parent1).

	test(category_2) :-
		implements_protocol(category_2, protocol1),
		implements_protocol(category_2, protocol2),
		extends_category(category_2, parent1),
		extends_category(category_2, parent2).

	test(category_3) :-
		implements_protocol(category_3, protocol1),
		implements_protocol(category_3, protocol2),
		extends_category(category_3, parent1),
		extends_category(category_3, parent2).

:- end_object.
