%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(test_object_1).

	:- public(p/1).
	p(1).

	:- public(q/2).
	q(1, foo).
	q(2, bar).

:- end_object.


:- object(test_object_2,
	implements(forwarding)).

	forward(Message) :-
		[test_object_1::Message].

:- end_object.


:- object(test_object_3,
	implements(forwarding)).

	forward(Message) :-
		call([test_object_1::Message], bar).

:- end_object.
