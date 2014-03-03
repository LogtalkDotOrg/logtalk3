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
		date is 2012/12/06,
		comment is 'Unit tests for the once/1 built-in method.'
	]).

	% once/1 calls are expanded and thus the error term is for call/1

	throws(once_1_1, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		once(_).

	throws(once_1_2, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
		Goal = 1,
		once(Goal).

	% it's not always possible to decompile the actual call

	throws(once_1_3, error(existence_error(procedure,_),logtalk(call(p(_)),This))) :-
		this(This),
		Goal = p(_),
		once(Goal).

	succeeds(once_1_4) :-
		findall(X, once(a(X)), Xs),
		Xs == [1].

	fails(once_1_5) :-
		once(a(4)).

	a(1).
	a(2).
	a(3).

:- end_object.
