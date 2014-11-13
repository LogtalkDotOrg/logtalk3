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

	% for supporting testing instantiation errors
	:- public(ie/1).
	ie(Object) :-
		Object::predicate_property(foo, _).

	% for supporting testing type errors
	:- public(te/0).
	te :-
		Object = 1,
		Object::predicate_property(foo, _).

	% for supporting testing meta-predicate properties
	:- public(meta/2).
	:- meta_predicate(meta(0, *)).

	% for supporting testing non-terminal properties
	:- public(nt//0).

:- end_object.
