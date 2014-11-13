%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(test_object).

	:- public(p/1).
	:- dynamic(p/1).

	:- protected(q/2).

	:- private(r/3).

	:- public(s/4).

	:- public(ie/1).
	ie(Object) :-
		Object::abolish(foo/1).

	:- public(te/0).
	te :-
		Object = 1,
		Object::abolish(foo/1).

:- end_object.
