%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


/*******************************************************
 * 99 Bottles of Beer
 * Paulo Moura - January 21, 2007
 * bottles.lgt
 * To execute start Logtalk and use the query
 * logtalk_load(bottles).
 *******************************************************/

:- object(bottles).

	:- initialization(sing(99)).

	sing(0) :-
		write('No more bottles of beer on the wall, no more bottles of beer.'), nl,
		write('Go to the store and buy some more, 99 bottles of beer on the wall.'), nl, nl.
	sing(N) :-
		N > 0,
		N2 is N - 1,
		beers(N), write(' of beer on the wall, '), beers(N), write(' of beer.'), nl,
		write('Take one down and pass it around, '), beers(N2), write(' of beer on the wall.'), nl, nl,
		sing(N2).

	beers(0) :-
		write('no more bottles').
	beers(1) :-
		write('1 bottle').
	beers(N) :-
		N > 1,
		write(N), write(' bottles').

:- end_object.
