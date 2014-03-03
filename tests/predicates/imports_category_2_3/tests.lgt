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
		date is 2013/03/11,
		comment is 'Unit tests for the imports_category/2-3 built-in predicates.'
	]).

	% imports_category/2 tests

	throws(imports_category_2_1, error(type_error(object_identifier, 1), logtalk(imports_category(1, _), _))) :-
		imports_category(1, _).

	throws(imports_category_2_2, error(type_error(category_identifier, 1), logtalk(imports_category(_, 1), _))) :-
		imports_category(_, 1).

	% imports_category/3 tests

	throws(imports_category_3_1, error(type_error(object_identifier, 1), logtalk(imports_category(1, _, _), _))) :-
		imports_category(1, _, _).

	throws(imports_category_3_2, error(type_error(category_identifier, 1), logtalk(imports_category(_, 1, _), _))) :-
		imports_category(_, 1, _).

	throws(imports_category_3_3, error(type_error(atom, 1), logtalk(imports_category(_, _, 1), _))) :-
		imports_category(_, _, 1).

	throws(imports_category_3_4, error(domain_error(scope, a), logtalk(imports_category(_, _, a), _))) :-
		imports_category(_, _, a).

:- end_object.
