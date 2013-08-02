
:- object(this_1_test_object_2).

	% secondary declaration for the p/1 multifile predicate:
	% a primary declaration with a public scope directive is
	% required for this secondary declaration to be valid
	:- multifile(this_1_test_object_1::p/1).
	:- dynamic(this_1_test_object_1::p/1).

	this_1_test_object_1::p(This) :-
		this(This).

:- end_object.
