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
		date is 2012/03/01,
		comment is 'Unit tests for the "patching" example.']).

	unit(broken).
	unit(metaclass).
	unit(broken_class).
	unit(instance).
	unit(patch).

	succeeds(patching_1) :-
		setof((Category, Object), complements_object(Category, Object), Pairs),
		Pairs == [(patch,broken), (patch,broken_class)].

	succeeds(patching_2) :-
		broken::is_proper_list([1,2,3]).

	succeeds(patching_3) :-
		instance::is_proper_list([1,2,3]).

	fails(patching_4) :-
		broken::is_proper_list(_).

	fails(patching_5) :-
		instance::is_proper_list(_).

	fails(patching_6) :-
		broken::is_proper_list([a,b,c|_]).

	fails(patching_6) :-
		instance::is_proper_list([a,b,c|_]).

	throws(patching_8, error(permission_error(access, private_predicate, last/3), _)) :- 
		broken::last(_, _, _).

	throws(patching_9, error(permission_error(access, private_predicate, last/3), _)) :- 
		instance::last(_, _, _).

:- end_object.
