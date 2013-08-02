
:- object(parameter_2_multifile_test_object_1(_)).

	% primary declaration for the p/1 multifile predicate:
	% the public scope directive is mandatory
	:- public(p/1).
	:- multifile(p/1).
	:- dynamic(p/1).

:- end_object.
