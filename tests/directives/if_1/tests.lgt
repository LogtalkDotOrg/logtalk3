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
		date is 2014/09/09,
		comment is 'Unit tests for the if/1 and other conditional compilation built-in directives.'
	]).

	:- uses(user, [aa/1, bb/1, cc/1, dd/1, ee/1, zz/1]).

	test(if_endif_0) :-
		aa(0).
	test(if_endif_1) :-
		aa(1).
	test(if_endif_2) :-
		\+ aa(2).

	test(if_else_endif_0) :-
		bb(0).
	test(if_else_endif_1) :-
		bb(1), \+ bb(2).
	test(if_else_endif_2) :-
		\+ bb(3), bb(4).

	test(if_elif_else_endif_0) :-
		cc(0).
	test(if_elif_else_endif_1) :-
		cc(1), \+ cc(2), \+ cc(3).
	test(if_elif_else_endif_2) :-
		cc(4), \+ cc(5), cc(6), \+ cc(7).
	test(if_elif_else_endif_3) :-
		cc(8), \+ cc(9), \+ cc(10), cc(11).
	test(if_elif_else_endif_4) :-
		cc(12), \+ cc(13), \+ cc(14), cc(15), \+ cc(16).

	test(if_if_endif_0) :-
		dd(0).
	test(if_if_endif_1) :-
		dd(1), dd(2),
		dd(3), \+ dd(4), 
		dd(5), dd(6), \+ dd(7),
		dd(8), \+ dd(9), dd(10),
		dd(11),
		\+ dd(12).

	test(if_elif_endif_0) :-
		ee(0).
	test(if_elif_endif_1) :-
		\+ ee(1), \+ ee(2),
		\+ ee(3), \+ ee(4), \+ ee(5),
		\+ ee(6), \+ ee(7), \+ ee(8), \+ ee(9),
		\+ ee(10),
		\+ ee(11),
		ee(12).

	test(if_end_of_file_0) :-
		zz(0).

:- end_object.
