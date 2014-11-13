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

	:- set_logtalk_flag(context_switching_calls, allow).

	% scoped operarors; seen by the reflection built-in methods
	:- public(op(501, xfx, abc)).
	:- protected(op(501, xfx, def)).
	:- private(op(501, xfx, ghi)).

	% local operator; invisible to the reflection built-in methods
	:- op(501, xfx, jkl).

	% test operator overriding
	:- public(op(600, xfx, (:))).

	% test for call in "self"
	:- public(operators/1).
	operators(Operators) :-
		setof(Operator, ::current_op(501, xfx, Operator), Operators).

	% tests for invalid object identifiers
	:- public(ie/1).
	ie(Object) :-
		Object::current_op(_, _, _).

	:- public(te/0).
	te :-
		Object = 1,
		Object::current_op(_, _, _).

:- end_object.


:- object(test_object_2,
	extends(test_object_1)).

	:- public(op(501, xfx, opq)).
	:- protected(op(501, xfx, rst)).
	:- private(op(501, xfx, uvw)).

	% local operator; invisible to the reflection built-in methods
	:- op(501, xfx, xyz).

:- end_object.
