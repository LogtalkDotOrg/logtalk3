%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(blind_search(_),
	instantiates(class),
	specializes(search_strategy)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Blind search state space strategies.',
		parnames is ['Bound']
	]).

	:- public(bound/1).
	:- mode(bound(?integer), zero_or_one).
	:- info(bound/1, [
		comment is 'Search depth bound.',
		argnames is ['Bound']
	]).

	:- protected(search/4).
	:- mode(search(+object, +nonvar, +integer, -list), zero_or_more).
	:- info(search/4, [
		comment is 'State space search solution.',
		argnames is ['Space', 'State', 'Bound', 'Path']
	]).

	bound(Bound) :-
		parameter(1, Bound).

	solve(Space, State, Path) :-
		::bound(Bound),
		::search(Space, State, Bound, Path).

:- end_object.
