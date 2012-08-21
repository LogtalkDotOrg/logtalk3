/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this file with other files,
    compiled with a Free Software compiler, to produce an executable,
    this file does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might
    be covered by the GNU General Public License.
*/

:- module(chrdif,[chrdif/2]).
:- use_module(library(chr)).

:- constraints dif/2, dif2/3, or/2, or_seq/2, del_or/1.

chrdif(X,Y) :- dif(X,Y).

dif(X,Y) <=> compound(X), compound(Y) | dif1(X,Y).
dif(X,X) <=> fail.
dif(X,Y) <=> nonvar(X), nonvar(Y) /* X \== Y holds */ | true.

dif1(X,Y) :-
	( functor(X,F,A),
	  functor(Y,F,A) ->
		X =.. [_|XL],
		Y =.. [_|YL],
		dif1l(XL,YL,A)
	;
		true
	).

dif1l(Xs,Ys,N) :-
	or(Or,N),
	dif1l_2(Xs,Ys,Or).

dif1l_2([],[],_).
dif1l_2([X|Xs],[Y|Ys],Or) :-
	dif2(X,Y,Or),
	dif1l_2(Xs,Ys,Or).

or_seq(OrP,Or) \ or(Or,0), or(OrP,N) <=> M is N - 1, or_seq(OrP,M).
or(_,0) <=> fail.

dif2(X,Y,Or) <=> compound(X), compound(Y) | dif3(X,Y,Or).
dif2(X,X,Or), or(Or,N) <=> M is N - 1, or(Or,M).
dif2(X,Y,Or) <=> nonvar(X), nonvar(Y) /* X \== Y holds */ | del_or(Or).

del_or(Or) \ or_seq(OrP,Or) <=> del_or(OrP).
del_or(Or) \ or_seq(Or,OrC) <=> del_or(OrC).
del_or(Or) \ or(Or,_) <=> true.
del_or(Or) \ dif2(_,_,Or) <=> true.
del_or(Or) <=> true.

dif3(X,Y,Or) :-
	( functor(X,F,A),
	  functor(Y,F,A) ->
		X =.. [_|XL],
		Y =.. [_|YL],
		or_seq(Or,Or2),
		dif1l(XL,YL,A)
	;
		del_or(Or)
	).
