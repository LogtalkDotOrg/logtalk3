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
		date is 2013/05/27,
		comment is 'Unit tests for the multifile/1 built-in directive.'
	]).

	:- multifile(multifile_test_object::m1/1).
	multifile_test_object::m1(3).

	:- multifile(multifile_test_object::m2/1).
	multifile_test_object::m2(3).

	test(multifile_1_1) :-
		findall(X, multifile_test_object::m1(X), L),
		L == [1, 2, 3].

	test(multifile_1_2) :-
		multifile_test_object::predicate_property(m1(_), (multifile)),
		multifile_test_object::predicate_property(m1(_), static).

	test(multifile_1_3) :-
		findall(X, multifile_test_object::m2(X), L),
		L == [1, 2, 3].

	test(multifile_1_4) :-
		multifile_test_object::predicate_property(m2(_), (multifile)),
		multifile_test_object::predicate_property(m2(_), (dynamic)).

:- end_object.
