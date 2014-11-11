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
		date is 2012/11/19,
		comment is 'Unit tests for the complements_object/2 built-in predicate.'
	]).

	throws(complements_object_2_1, error(type_error(category_identifier, 1), logtalk(complements_object(1, _), _))) :-
		complements_object(1, _).

	throws(complements_object_2_2, error(type_error(object_identifier, 1), logtalk(complements_object(_, 1), _))) :-
		complements_object(_, 1).

:- end_object.
