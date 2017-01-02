%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
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
