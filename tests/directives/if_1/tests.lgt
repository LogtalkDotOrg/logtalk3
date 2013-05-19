
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/19,
		comment is 'Unit tests for the if/1 and other conditional compilation built-in directives.'
	]).

	:- uses(user, [a/1, b/1, c/1, d/1, e/1, z/1]).

	test(if_endif_0) :-
		a(0).
	test(if_endif_1) :-
		a(1).
	test(if_endif_2) :-
		\+ a(2).

	test(if_else_endif_0) :-
		b(0).
	test(if_else_endif_1) :-
		b(1), \+ b(2).
	test(if_else_endif_2) :-
		\+ b(3), b(4).

	test(if_elif_else_endif_0) :-
		c(0).
	test(if_elif_else_endif_1) :-
		c(1), \+ c(2), \+ c(3).
	test(if_elif_else_endif_2) :-
		c(4), \+ c(5), c(6), \+ c(7).
	test(if_elif_else_endif_3) :-
		c(8), \+ c(9), \+ c(10), c(11).
	test(if_elif_else_endif_4) :-
		c(12), \+ c(13), \+ c(14), c(15), \+ c(16).

	test(if_if_endif_0) :-
		d(0).
	test(if_if_endif_1) :-
		d(1), d(2),
		d(3), \+ d(4), 
		d(5), d(6), \+ d(7),
		d(8), \+ d(9), d(10),
		d(11),
		\+ d(12).

	test(if_elif_endif_0) :-
		e(0).
	test(if_elif_endif_1) :-
		\+ e(1), \+ e(2),
		\+ e(3), \+ e(4), \+ e(5),
		\+ e(6), \+ e(7), \+ e(8), \+ e(9),
		\+ e(10),
		\+ e(11),
		e(12).

	test(if_end_of_file_0) :-
		z(0).

:- end_object.
