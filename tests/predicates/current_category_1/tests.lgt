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
		date is 2013/05/04,
		comment is 'Unit tests for the current_category/1 built-in predicate.'
	]).

	throws(current_category_1_1, error(type_error(category_identifier, 1), logtalk(current_category(1), _))) :-
		current_category(1).

	fails(current_category_1_2) :-
		current_category(non_exisiting_category).

	succeeds(current_category_1_3) :-
		current_category(core_messages),
		category_property(core_messages, built_in),
		category_property(core_messages, static).

:- end_object.
