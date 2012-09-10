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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2011/02/01,
		comment is 'Unit tests for the "delegates" example.']).

	unit(delegator).
	unit(a_delegator).
	unit(a_delegator(_)).
	unit(delegate).
	unit(a_delegate).
	unit(an_object).

	% without a delegate:
	test(delegates_1) :-
		a_delegator::operation(String),
		String == 'default implementation'.

	% with a delegate that does not implement thing/1:
	test(delegates_2) :-
		a_delegator::set_delegate(an_object),
		a_delegator::operation(String),
		String == 'default implementation'.

	% with a delegate that implements thing/1:
	test(delegates_3) :-
		a_delegator::set_delegate(a_delegate),
		a_delegator::operation(String),
		String == 'delegate implementation'.

	% same tests but using the parametric object implementation:
	test(delegates_4) :-
		a_delegator(an_object)::operation(String),
		String == 'default implementation'.

	test(delegates_5) :-
		a_delegator(a_delegate)::operation(String),
		String == 'delegate implementation'.

:- end_object.
