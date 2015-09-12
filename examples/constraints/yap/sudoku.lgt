%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
% written by Markus Triska (August 2008)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sudoku CLP(FD) animation.

   Written Feb. 2008 by Markus Triska  (triska@gmx.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- object(soduku).

	:- use_module(library(clpfd), [all_different/1, ins/2, labeling/2]).
	:- use_module(library(system), [popen/3]).

	:- uses(list, [append/2, length/2]).
	:- uses(meta, [succeeds/2::maplist/2]).

	:- public([problem/2, show/2, sudoku/1]).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Constraint posting
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	sudoku(Rows) :-
		length(Rows, 9), maplist(length_(9), Rows),
		append(Rows, Vs), Vs ins 1..9,
		maplist(clpfd:all_different, Rows),
		transpose(Rows, Columns), maplist(clpfd:all_different, Columns),
		Rows = [A,B,C,D,E,F,G,H,I],
		blocks(A, B, C), blocks(D, E, F), blocks(G, H, I).

	length_(L, Ls) :- length(Ls, L).

	transpose(Ms, Ts) :- Ms = [F|_], transpose(F, Ms, Ts).

	transpose([], _, []).
	transpose([_|Rs], Ms, [Ts|Tss]) :-
		lists_firsts_rests(Ms, Ts, Ms1),
		transpose(Rs, Ms1, Tss).

	lists_firsts_rests([], [], []).
	lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
		lists_firsts_rests(Rest, Fs, Oss).

	blocks([], [], []).
	blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
		all_different([A,B,C,D,E,F,G,H,I]),
		blocks(Bs1, Bs2, Bs3).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Animation.

	   A frozen goal for each variable emits PostScript instructions to
	   draw a number. On backtracking, the field is cleared.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	animate(Rows) :- animate(Rows, 1).

	animate([], _).
	animate([Row|Rows], N) :-
		animate(Row, 1, N),
		N1 is N + 1,
		animate(Rows, N1).

	animate([], _, _).
	animate([C|Cs], Col, Row) :-
		freeze(C, label(Col, Row, C)),
		Col1 is Col + 1,
		animate(Cs, Col1, Row).

	label(Col, Row, N) :- format("(~w) ~w ~w num\n", [N,Col,Row]).
	label(Col, Row, _) :- format("~w ~w clear\n", [Col,Row]), fail.

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   PostScript definitions. Place a number N and clear a cell with:

	   (N) Col Row num
	   Col Row clear
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	postscript -->
		"systemdict /.setlanguagelevel known { 2 .setlanguagelevel} if \
		/Palatino-Bold findfont 5 scalefont setfont \
			320 9 div dup scale 0 setlinewidth -0.9 -0.9 translate \
		/num { gsave 10 exch sub translate 0.5 0.25 translate 0.16 dup scale \
			dup stringwidth pop -2 div 0 moveto show grestore } bind def \
		/clear { gsave 10 exch sub translate 1 setgray 0.1 dup 0.8 dup rectfill \
			grestore } bind def \
		1 1 10 { gsave dup 1 moveto 10 lineto stroke grestore } for \
		1 1 10 { gsave dup 1 exch moveto 10 exch lineto stroke grestore } for \
		1 3 9 { 1 3 9 { 1 index gsave translate 0.05 setlinewidth \
			0 0 3 3 rectstroke grestore } for pop } for\n".

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Putting it all together: Set up communication with gs.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	show(Options, Rows) :-
		sudoku(Rows),
		popen('gs -dNOPAUSE -g680x680 -dGraphicsAlphaBits=2 -r150 -q -', write, Out),
		tell(Out),
		phrase(postscript, Ps),
		format(Ps, []),
		append(Rows, Vs),
		call_cleanup((animate(Rows),labeling(Options, Vs),finish), close(Out)).

	finish :-
		format("copypage\n", []),
		% fill the buffer to make 'gs' process all generated output
		ignore((between(1,500,_),
			format("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n", []),
			fail)),
		flush_output.

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Sample problems.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	problem(1, P) :- % shokyuu
		P = [[1,_,_,8,_,4,_,_,_],
			 [_,2,_,_,_,_,4,5,6],
			 [_,_,3,2,_,5,_,_,_],
			 [_,_,_,4,_,_,8,_,5],
			 [7,8,9,_,5,_,_,_,_],
			 [_,_,_,_,_,6,2,_,3],
			 [8,_,1,_,_,_,7,_,_],
			 [_,_,_,1,2,3,_,8,_],
			 [2,_,5,_,_,_,_,_,9]].

	problem(2, P) :-  % shokyuu
		P = [[_,_,2,_,3,_,1,_,_],
			 [_,4,_,_,_,_,_,3,_],
			 [1,_,5,_,_,_,_,8,2],
			 [_,_,_,2,_,_,6,5,_],
			 [9,_,_,_,8,7,_,_,3],
			 [_,_,_,_,4,_,_,_,_],
			 [8,_,_,_,7,_,_,_,4],
			 [_,9,3,1,_,_,_,6,_],
			 [_,_,7,_,6,_,5,_,_]].

	problem(3, P) :-
		P = [[1,_,_,_,_,_,_,_,_],
			 [_,_,2,7,4,_,_,_,_],
			 [_,_,_,5,_,_,_,_,4],
			 [_,3,_,_,_,_,_,_,_],
			 [7,5,_,_,_,_,_,_,_],
			 [_,_,_,_,_,9,6,_,_],
			 [_,4,_,_,_,6,_,_,_],
			 [_,_,_,_,_,_,_,7,1],
			 [_,_,_,_,_,1,_,3,_]].

	%?- show([ff], Rows).

	%?- problem(1, Rows), show([ff], Rows).

:- end_object.
