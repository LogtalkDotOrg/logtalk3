
:- object(multifile_test_object).

	:- public(m1/1).
	:- multifile(m1/1).
	m1(1). m1(2).

	:- public(m2/1).
	:- multifile(m2/1).
	:- dynamic(m2/1).
	m2(1). m2(2).

:- end_object.
