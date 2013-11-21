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
		comment is 'Unit tests for the instantiates_class/2-3 built-in predicates.'
	]).

	% instantiates_class/2 tests

	throws(instantiates_class_2_1, error(type_error(object_identifier, 1), logtalk(instantiates_class(1, _), _))) :-
		instantiates_class(1, _).

	throws(instantiates_class_2_2, error(type_error(object_identifier, 1), logtalk(instantiates_class(_, 1), _))) :-
		instantiates_class(_, 1).

	% instantiates_class/3 tests

	throws(instantiates_class_3_1, error(type_error(object_identifier, 1), logtalk(instantiates_class(1, _, _), _))) :-
		instantiates_class(1, _, _).

	throws(instantiates_class_3_2, error(type_error(object_identifier, 1), logtalk(instantiates_class(_, 1, _), _))) :-
		instantiates_class(_, 1, _).

	throws(instantiates_class_3_3, error(type_error(atom, 1), logtalk(instantiates_class(_, _, 1), _))) :-
		instantiates_class(_, _, 1).

	throws(instantiates_class_3_4, error(domain_error(scope, a), logtalk(instantiates_class(_, _, a), _))) :-
		instantiates_class(_, _, a).

:- end_object.
