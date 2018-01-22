%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% written by Markus Triska (November 2009)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Closed Knight's Tour.

   Written by Markus Triska (triska@gmx.at) Nov. 2nd 2009
   Tested with SWI-Prolog 5.9.0
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- object(knight).

	:- use_module(clpfd, [
					op(700, xfx, #=), op(700, xfx, in), op(700, xfx, ins),
					(#=)/2, circuit/1, (in)/2, (ins)/2, label/1]).

	:- uses(list, [append/2, length/2, nth1/3]).
	:- uses(meta, [map/2::maplist/2]).

	:- public(n_tour/2).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Constraints.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	n_tour(N, Ts) :-
		length(Ts, N),
		maplist(length_(N), Ts),
		append(Ts, Vs),
		circuit(Vs),
		successors(Vs, N, 1).

	successors([], _, _).
	successors([V| Vs], N, K0) :-
		findall(Num, n_k_next(N, K0, Num), [Next| Nexts]),
		nums_to_dom(Nexts, Next, Dom),
		V in Dom,
		K1 #= K0 + 1,
		successors(Vs, N, K1).

	nums_to_dom([], D, D).
	nums_to_dom([N| Ns], D0, D) :-
		nums_to_dom(Ns, D0 \/ N, D).

	length_(L, Ls) :-
		length(Ls, L).

	n_x_y_k(N, X, Y, K) :-
		[X,Y] ins 1..N,
		K #= N*(Y-1) + X.

	n_k_next(N, K, Next) :-
		n_x_y_k(N, X0, Y0, K),
		[DX,DY] ins -2 \/ -1 \/ 1 \/ 2,
		abs(DX) + abs(DY) #= 3,
		[X,Y] ins 1..N,
		X #= X0 + DX,
		Y #= Y0 + DY,
		n_x_y_k(N, X, Y, Next),
		label([DX, DY]).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   	Text display.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	:- public(tour_enumeration/2).

	tour_enumeration(Ts, Es) :-
		length(Ts, N),
		length(Es, N),
		maplist(length_(N), Es),
		append(Ts, Vs),
		append(Es, Ls),
		vs_enumeration(Vs, Vs, 1, 1, Ls).

	vs_enumeration([], _, _, _, _).
	vs_enumeration([_| Rest], Vs, V0, E0, Ls) :-
		nth1(V0, Ls, E0),
		nth1(V0, Vs, V1),
		E1 #= E0 + 1,
		vs_enumeration(Rest, Vs, V1, E1, Ls).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PostScript display. Requires Ghostscript ("gs").
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	antescript -->
		"/init {  5 5 translate  \
		   /N exch def 500 N div dup /scalefactor exch def dup scale \
		   2 scalefactor div setlinewidth 0 0 N N rectstroke  \
		   1 1 N { dup 0 moveto N lineto stroke } for  \
		   1 1 N { dup 0 exch moveto N exch lineto stroke } for \
		   0 0 1 setrgbcolor -.5 dup translate 1 N moveto } bind def \
		 /jump { N exch sub 1 add lineto gsave currentpoint newpath \
		         4 scalefactor div 0 360 arc fill grestore } bind def".

	tour_postscript(Ts) :-
		phrase(antescript, As),
		length(Ts, N),
		format("~s ~w init\n", [As, N]),
		append(Ts, Vs),
		vs_postscript(Vs, Vs, N, 1),
		format("stroke\n").

	vs_postscript([], _, _, _).
	vs_postscript([_| Rest], Vs, N, V0) :-
		nth1(V0, Vs, V1),
		n_x_y_k(N, X, Y, V1),
		format("~w ~w jump\n", [X, Y]),
		vs_postscript(Rest, Vs, N, V1).

	:- public(show/1).

	show(Ts) :-
		setup_call_cleanup(
			open(pipe('gs -dNOPROMPT -g510x510 -dGraphicsAlphaBits=2 -r72 -q'), write, Out, [buffer(false)]),
			(tell(Out), tour_postscript(Ts), (Sol = yes; Sol = no), Sol == yes),
			close(Out)
		).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Examples:
	See the "SCRIPT.txt" file for usage examples.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- end_object.
