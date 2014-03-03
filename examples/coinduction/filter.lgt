%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(filter).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/07/02,
		comment is 'Coinduction example of filtering a coinductive list.'
	]).

	:- public(filter/2).
	:- coinductive(filter/2).

	filter([H| T], [H| T2]) :-
		even(H),
		filter(T, T2).
	filter([H| T], L2) :-
		\+ even(H),
		filter(T, L2).

	even(0).
	even(s(s(N))) :-
		even(N).

:- end_object.
