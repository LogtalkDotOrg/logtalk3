%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(symdiffp).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		comment is 'Symbolic differentiation and simplification protocol.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	:- public(diff/1).
	:- mode(diff(-expression), one).
	:- info(diff/1, [
		comment is 'Returns the symbolic differentiation of self.',
		argnames is ['Expression']
	]).

	:- public(simplify/1).
	:- mode(simplify(-expression), one).
	:- info(simplify/1, [
		comment is 'Returns the symbolic simplification of self.',
		argnames is ['Expression']
	]).

:- end_protocol.



:- object(x,
	implements(symdiffp)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		comment is 'Symbolic differentiation and simplification of a variable.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	diff(1).

	simplify(x).

:- end_object.



:- object(_ + _,
	implements(symdiffp)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression1', 'Expression2'],
		comment is 'Symbolic differentiation and simplification of +/2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	diff(Diff) :-
		this(X + Y),
		once(diff(X, Y, Diff)).

	diff(I, J, 0) :-
		integer(I),
		integer(J).

	diff(X, J, DX) :-
		integer(J),
		X::diff(DX).

	diff(I, Y, DY) :-
		integer(I),
		Y::diff(DY).

	diff(X, Y, DX + DY) :-
		X::diff(DX),
		Y::diff(DY).


	simplify(S) :-
		this(X + Y),
		once(simplify(X, Y, S)).


	simplify(I, J, S) :-
		integer(I),
		integer(J),
		S is I + J.

	simplify(X, 0, S) :-
		X::simplify(S).

	simplify(0, Y, S) :-
		Y::simplify(S).

	simplify(X, J, S + J) :-
		integer(J),
		X::simplify(S).

	simplify(I, Y, I + S) :-
		integer(I),
		Y::simplify(S).

	simplify(X, Y, S) :-
		X::simplify(SX),
		Y::simplify(SY),
		(	X + Y \= SX + SY ->
			(SX + SY)::simplify(S)
		;	S = SX + SY
		).

:- end_object.



:- object(_ - _,
	implements(symdiffp)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression1', 'Expression2'],
		comment is 'Symbolic differentiation and simplification of -/2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	diff(Diff) :-
		this(X - Y),
		once(diff(X, Y, Diff)).

	diff(I, J, 0) :-
		integer(I),
		integer(J).

	diff(X, J, DX) :-
		integer(J),
		X::diff(DX).

	diff(I, Y, DY) :-
		integer(I),
		Y::diff(DY).

	diff(X, Y, DX - DY) :-
		X::diff(DX),
		Y::diff(DY).


	simplify(S) :-
		this(X - Y),
		once(simplify(X, Y, S)).


	simplify(X, X, 0).

	simplify(I, J, S) :-
		integer(I),
		integer(J),
		S is I - J.

	simplify(X, 0, S) :-
		X::simplify(S).

	simplify(0, Y, S) :-
		Y::simplify(S).

	simplify(X, J, S - J) :-
		integer(J),
		X::simplify(S).

	simplify(I, Y, I - S) :-
		integer(I),
		Y::simplify(S).

	simplify(X, Y, S) :-
		X::simplify(SX),
		Y::simplify(SY),
		(	X - Y \= SX - SY ->
			(SX - SY)::simplify(S)
		;	S = SX - SY
		).

:- end_object.



:- object(_ * _,
	implements(symdiffp)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression1', 'Expression2'],
		comment is 'Symbolic differentiation and simplification of */2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	diff(Diff) :-
		this(X * Y),
		once(diff(X, Y, Diff)).

	diff(I, J, 0) :-
		integer(I),
		integer(J).

	diff(0, _, 0).

	diff(_, 0, 0).

	diff(X, J, J * DX) :-
		integer(J),
		X::diff(DX).

	diff(I, Y, I * DY) :-
		integer(I),
		Y::diff(DY).

	diff(X, Y, X * DY + DX * Y) :-
		X::diff(DX),
		Y::diff(DY).


	simplify(S) :-
		this(X * Y),
		once(simplify(X, Y, S)).


	simplify(I, J, S) :-
		integer(I),
		integer(J),
		S is I * J.

	simplify(0, _, 0).

	simplify(_, 0, 0).

	simplify(1, Y, SY) :-
		Y::simplify(SY).

	simplify(X, 1, SX) :-
		X::simplify(SX).

	simplify(I, Y, I * SY) :-
		integer(I),
		Y::simplify(SY).

	simplify(X, J, J * SX) :-
		integer(J),
		X::simplify(SX).

	simplify(X, Y, SX * SY) :-
		X::simplify(SX),
		Y::simplify(SY).

:- end_object.



:- object(_ ** _,
	implements(symdiffp)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression', 'Power'],
		comment is 'Symbolic differentiation and simplification of **/2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	diff(Diff) :-
		this(X ** Y),
		once(diff(X, Y, Diff)).

	diff(X, Y, Y * X ** Y2 * DX) :-
		integer(Y),
		Y2 is Y - 1,
		X::diff(DX).

	diff(X, Y, Y * X ** Y2 * DX) :-
		Y2 = Y - 1,
		X::diff(DX).


	simplify(S) :-
		this(X ** Y),
		once(simplify(X, Y, S)).


	simplify(_, 0, 1).

	simplify(X, 1, X).

	simplify(X, Y, S ** Y) :-
		integer(Y),
		X::simplify(S).

	simplify(X, Y, SX ** SY) :-
		X::simplify(SX),
		Y::simplify(SY).

:- end_object.



:- object(log(_),
	implements(symdiffp)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression'],
		comment is 'Symbolic differentiation and simplification of log/1 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.'
	]).

	diff(Diff) :-
		this(log(X)),
		once(diff(X, Diff)).

	diff(I, 0) :-
		integer(I).

	diff(X, DX * X ** -1) :-
		X::diff(DX).

	simplify(S) :-
		this(log(X)),
		once(simplify(X, S)).

	simplify(1, 0).

	simplify(I, Log) :-
		integer(I),
		Log is log(I).

	simplify(X, X).

:- end_object.
