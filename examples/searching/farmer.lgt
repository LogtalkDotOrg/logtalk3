%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(farmer,
	instantiates(state_space)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Farmer, cabbage, goat, and wolf state space search problem.'
	]).

	initial_state(start, (north, north, north, north)).

	goal_state(end, (south, south, south, south)).

	next_state((Cabbage, Goat, Wolf, Farmer), (FCabbage, Goat, Wolf, FFarmer)) :-
		same_side(Farmer, Cabbage),
		\+ same_side(Goat, Wolf),
		flip(Farmer, FFarmer),
		flip(Cabbage, FCabbage).
	next_state((Cabbage, Goat, Wolf, Farmer), (Cabbage, FGloat, Wolf, FFarmer)) :-
		same_side(Farmer, Goat),
		flip(Farmer, FFarmer),
		flip(Goat, FGloat).
	next_state((Cabbage, Goat, Wolf, Farmer), (Cabbage, Goat, FWolf, FFarmer)) :-
		same_side(Farmer, Wolf),
		\+ same_side(Cabbage, Goat),
		flip(Farmer, FFarmer),
		flip(Wolf, FWolf).
	next_state((Cabbage, Goat, Wolf, Farmer), (Cabbage, Goat, Wolf, FFarmer)) :-
		\+ same_side(Cabbage, Goat),
		\+ same_side(Goat, Wolf),
		flip(Farmer, FFarmer).

	flip(north, south).
	flip(south, north).

	same_side(north, north).
	same_side(south, south).

	print_state((Cabbage, Goat, Wolf, Farmer)) :-
		(Cabbage = north -> write(c); write('_')),
		(Goat = north -> write(g); write('_')),
		(Wolf = north -> write(w); write('_')),
		(Farmer = north -> write('f.<__>.........._'); write('_..........<__>.f')),
		(Cabbage = north -> write('_'); write(c)),
		(Goat = north -> write('_'); write(g)),
		(Wolf = north -> write('_'); write(w)),
		nl.

:- end_object.
