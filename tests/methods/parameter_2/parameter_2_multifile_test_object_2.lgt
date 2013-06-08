
:- object(parameter_2_multifile_test_object_2).

	:- multifile(parameter_2_multifile_test_object_1(_)::p/1).
	parameter_2_multifile_test_object_1(_)::p(Parameter) :-
		parameter(1, Parameter).

:- end_object.
