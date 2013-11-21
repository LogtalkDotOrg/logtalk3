%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(forward_test_object_1).

	:- public(p/1).
	p(1).

	:- public(q/2).
	q(1, foo).
	q(2, bar).

:- end_object.



:- object(forward_test_object_2,
	implements(forwarding)).

	forward(Message) :-
		[forward_test_object_1::Message].

:- end_object.



:- object(forward_test_object_3,
	implements(forwarding)).

	forward(Message) :-
		call([forward_test_object_1::Message], bar).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/02/01,
		comment is 'Unit tests for the forward/1 built-in method.'
	]).

	test(forward_1_1) :-
		{forward_test_object_2::p(X)},
		X == 1.

	test(forward_1_2) :-
		{forward_test_object_3::q(X)},
		X == 2.

:- end_object.
