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
		date is 2012/12/12,
		comment is 'Unit tests for the abolish_category/1 built-in predicate.'
	]).

	throws(abolish_category_1_1, error(instantiation_error, logtalk(abolish_category(_), _))) :-
		abolish_category(_).

	throws(abolish_category_1_2, error(type_error(category_identifier, 1), logtalk(abolish_category(1), _))) :-
		abolish_category(1).

	throws(abolish_category_1_3, error(existence_error(category, non_exisiting_category), logtalk(abolish_category(non_exisiting_category), _))) :-
		abolish_category(non_exisiting_category).

	succeeds(abolish_category_1_4) :-
		create_category(Category, [], [], []),
		current_category(Category),
		abolish_category(Category),
		\+ current_category(Category).

	succeeds(abolish_category_1_5) :-
		create_category(a_category, [], [], []),
		current_category(a_category),
		abolish_category(a_category),
		\+ current_category(a_category).

:- end_object.
