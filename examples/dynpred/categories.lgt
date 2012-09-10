%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(ctg).					% categories are fine-grained units of code reuse
									% that can be imported by any number of objects
	:- public(get_default/1).
	:- public(set_default/1).

	:- public(get_value/1).
	:- public(set_value/1).

	:- private(state/1).			% categories can declare and handle dynamic
	:- dynamic(state/1).			% predicates but cannot contain clauses for them

	get_default(State) :-
		state(State).				% called in the context of "this"

	set_default(State) :-
		retractall(state(_)),		% retracts clauses in "this"
		assertz(state(State)).		% asserts clause in "this"

	get_value(State) :-
		::state(State).				% called in the context of "self"

	set_value(State) :-
		::retractall(state(_)),		% retracts clauses in "self"
		::assertz(state(State)).	% asserts clause in "self"

:- end_category.


:- object(top,						% category predicates are inherited
	imports(ctg)).					% by the descendants of the object
									% importing the category
:- end_object.


:- object(middle,
	extends(top)).

:- end_object.


:- object(bottom,
	extends(middle)).

:- end_object.
