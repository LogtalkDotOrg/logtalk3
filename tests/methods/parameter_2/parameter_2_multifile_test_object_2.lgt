
:- object(parameter_2_multifile_test_object_2).

	% secondary declaration for the p/1 multifile predicate:
	% a primary declaration with a public scope directive is
	% required for this secondary declaration to be valid
	:- multifile(parameter_2_multifile_test_object_1(_)::p/1).
	:- dynamic(parameter_2_multifile_test_object_1(_)::p/1).
	
	parameter_2_multifile_test_object_1(_)::p(Parameter) :-
		parameter(1, Parameter).

:- end_object.
