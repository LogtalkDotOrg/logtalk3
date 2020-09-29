%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
   N Queens animation demo.

   Written Feb. 2008 by Markus Triska (triska@gmx.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- object(queens).

	:- use_module(clpfd, [
		op(450, xfx, ..), op(700, xfx, #=), op(700, xfx, #\=),
		op(760, yfx, #<==>), op(700, xfx, #>), op(700, xfx, ins),
		(#=)/2, (#\=)/2, (#<==>)/2, (#>)/2, (ins)/2, labeling/2
	]).

	:- public([
		n_queens/2, show/3
	]).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Constraint posting
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	n_queens(N, Qs) :-
		length(Qs, N),
		Qs ins 1..N,
		safe_queens(Qs).

	safe_queens([]).
	safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

	safe_queens([], _, _).
	safe_queens([Q|Qs], Q0, D0) :-
		Q0 #\= Q,
		abs(Q0 - Q) #\= D0,
		D1 is D0 + 1,
		safe_queens(Qs, Q0, D1).


	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Animation.

	   For each N of the domain of queen Q, a reified constraint of the form

	      Q #= N #<==> B

	   is posted. When N vanishes from the domain, B becomes 0. A frozen
	   goal then emits PostScript instructions for graying out the field.
	   When B becomes 1, the frozen goal emits instructions for placing
	   the queen. On backtracking, the field is cleared.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	animate(Qs) :- length(Qs, N), animate(N, Qs).

	animate(0, _)  :- !.
	animate(N, Qs) :-
		animate_(Qs, 1, N),
		N1 is N - 1,
		animate(N1, Qs).

	animate_([], _, _).
	animate_([Q|Qs], C, N) :-
		freeze(B, queen_value_truth(C,N,B)),
		Q #= N #<==> B,
		C1 is C + 1,
		animate_(Qs, C1, N).

	queen_value_truth(Q, N, 1) :- format("~w ~w q\n", [Q,N]).
	queen_value_truth(Q, N, 0) :- format("~w ~w i\n", [Q,N]).
	queen_value_truth(Q, N, _) :- format("~w ~w c\n", [Q,N]), fail.


	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   PostScript definitions.

	   Sample instructions, with these definitions loaded:

	   2 init   % initialize a 2x2 board
	   1 1 q    % place a queen on 1-1
	   1 2 q
	   1 2 c    % remove the queen from 1-2
	   2 2 i    % gray out 2-2
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	postscript -->
		"systemdict /.setlanguagelevel known { 2 .setlanguagelevel} if \c
		/init {  /N exch def 322 N div dup scale -1 -1 translate \c
	          /Palatino-Roman findfont 0.8 scalefont setfont \c
	          0 setlinewidth \c
	          1 1 N { 1 1 N { 1 index c } for pop } for } bind def \c
		/showtext { 0.5 0.28 translate dup stringwidth pop -2 div 0 moveto \c
	          1 setgray show} bind def \c
		/i { gsave translate .5 setgray 0 0 1 1 4 copy rectfill 0 setgray rectstroke \c
	    	grestore } bind def \c
		/q { gsave translate 0 0 1 1 rectfill (Q) showtext grestore } bind def \c
		/c { gsave translate 1 setgray 0 0 1 1 4 copy rectfill 0 setgray rectstroke \c
			grestore } bind def\n".


	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Putting it all together: Set up communication with gs.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	show(N, Options, Qs) :-
		N #> 0,
		n_queens(N, Qs),
		open(pipe('gs -dNOPAUSE -g680x680 -dGraphicsAlphaBits=2 -r151 -q -'), write, Out, [buffer(false)]),
		set_output(Out),
		phrase(postscript, Ps),
		format("~s\n~w init\n", [Ps, N]),
		call_cleanup(((animate(Qs),labeling(Options, Qs),finish)), close(Out)).

	finish :-
		format("copypage\n"),
		% fill the buffer to make 'gs' process all generated output
		ignore((
			between(1,500,_),
			format("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"),
			fail
		)),
		flush_output.

	%?- show(N, [ff], Qs).

	%?- show(8, [ff], Qs).
	%@ Qs = [1, 5, 8, 6, 3, 7, 2, 4] .

:- end_object.
