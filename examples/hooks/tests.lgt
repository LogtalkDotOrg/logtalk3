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
		date is 2011/05/04,
		comment is 'Unit tests for the "hooks" example.'
	]).

	throws(hooks_1, error(permission_error(access, private_predicate, item/1), logtalk(object::item(_), This))) :-
		this(This),
		object::item(_).

	succeeds(hooks_2) :-
		object::items(Items),
		Items == [alpha, omega, zeta].

:- end_object.
