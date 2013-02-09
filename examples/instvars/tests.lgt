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
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/06,
		comment is 'Unit tests for the "instvars" example.'
	]).

	unit(root).
	unit(instance1).
	unit(instance2).
	unit(instance3).

	test(instvars_1) :-
		instance1::ivar(Value1), instance2::ivar(Value2), instance3::ivar(Value3),
		Value1 == 0, Value2 == 0, Value3 == 0.

	test(instvars_2) :-
		instance1::set_ivar(1).

	test(instvars_3) :-
		instance1::ivar(Value1), instance2::ivar(Value2), instance3::ivar(Value3),
		Value1 == 1, Value2 == 0, Value3 == 0.

:- end_object.
