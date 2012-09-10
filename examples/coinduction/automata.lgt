%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(automaton).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2012/08/17,
		comment is 'Coinduction omega-automaton example.']).

	:- public(automaton/2).
	:- coinductive(automaton/2).

	automaton(State, [Input| Inputs]) :-
		trans(State, Input, NewState),
		automaton(NewState, Inputs).
%	automata(State, []) :-		% we drop the base case in order
%		final(State).			% to get an omega-automaton

	trans(s0, a, s1).
	trans(s1, b, s2).
	trans(s2, c, s3).
	trans(s2, e, s0).
	trans(s3, d, s0).

	final(s2).

:- end_object.
