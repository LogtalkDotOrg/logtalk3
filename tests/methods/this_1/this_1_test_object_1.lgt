
:- object(this_1_test_object_1).

	% primary declaration for the p/1 multifile predicate:
	% the public scope directive is mandatory
	:- public(p/1).
	:- multifile(p/1).
	:- dynamic(p/1).

:- end_object.
