
:- object(this_1_test_object_2).

	:- multifile(this_1_test_object_1::p/1).
	this_1_test_object_1::p(This) :-
		this(This).

:- end_object.
