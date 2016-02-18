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


:- protocol(find_rootp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura and Paulo Nunes',
		date is 2006/11/26,
		comment is 'Default protocol for root find algorithms.'
	]).

	:- public(find_root/5).
	:- mode(find_root(+object_identifier, +float, +float, +float, -float), one).
	:- info(find_root/5, [
		comment is 'Find the root of a function in the interval [A, B] given a maximum aproximation error.',
		argnames is ['Function', 'A', 'B', 'Error', 'Zero']
	]).

:- end_protocol.



:- protocol(functionp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura and Paulo Nunes',
		date is 2006/11/26,
		comment is 'Default protocol for real functions of a single real variable.'
	]).

	:- public(eval/2).
	:- mode(eval(+float, -float), one).
	:- info(eval/2, [
		comment is 'Calculates the function value.',
		argnames is ['X', 'Fx']
	]).

	:- public(evald/2).
	:- mode(evald(+float, -float), one).
	:- info(evald/2, [
		comment is 'Calculates the value of the function derivative.',
		argnames is ['X', 'DFx']
	]).

:- end_protocol.



:- object(f1,
	implements(functionp)).

	% x^2 - 4
	% 2.0

	eval(X, Y) :-
		Y is X * X - 4.

	evald(X, Y) :-
		Y is 2 * X.

:- end_object.



:- object(f2,
	implements(functionp)).

	% x^7 + 9x^5 - 13x - 17
	% 1.29999999999945448

	eval(X, Y) :-
		Y is X**7 + 9*X**5 - 13*X - 17.

	evald(X, Y) :-
		Y is 7*X**6 + 45*X**4 - 13.

:- end_object.



:- object(f3,
	implements(functionp)).

	% (x - sqrt(2))^7
	% 1.41421356237309537

	eval(X, Y) :-
		Y is (X - sqrt(2.0))**8.

	evald(X, Y) :-
		Y is 8*(X - sqrt(2.0))**7.

:- end_object.



:- object(f4,
	implements(functionp)).

	% x + x^2*sin(2.0/x)
	% 0.0

	eval(X, Y) :-
		Y is X + (X**2)*sin(2.0/X).

	evald(X, Y) :-
		Y is 1 + 2*X*sin(2.0/X) - 2*cos(2.0/X).

:- end_object.



:- object(humps,
	implements(functionp)).

	% The Matlab humps function
	% 1.29954968258482

	eval(X, Y) :-
		Y is 1.0 / ((X-0.3)**2+0.01) + 1.0 / ((X-0.9)**2 + 0.04) - 6.0.

	evald(X, Y) :-
		Y is -1.0 / ((X-0.3)**2+0.01)**2 * (2*X - 3.0 / 5.0) -1.0 / ((X-0.9)**2+0.04)**2 * (2*X-1.8).

:- end_object.



:- object(bisection,
	implements(find_rootp)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura and Paulo Nunes',
		date is 2008/2/4,
		comment is 'Bisection algorithm.'
	]).

	find_root(Function, A, B, Error, Zero) :-
		Function::eval(A, Fa),
		Function::eval(B, Fb),
		(	Fa > 0.0, Fb < 0.0 ->
			true
		;	Fa < 0.0, Fb > 0.0
		),
		X0 is (A + B) / 2.0,
		bisection(Function, A, B, X0, Error, Zero).

	bisection(_, An, Bn, Xn, Error, Xn) :-
		abs(An-Bn) < 2*Error,
		!.
	bisection(Function, An, Bn, _, Error, Zero) :-
		Xn1 is (An + Bn) / 2.0,
		Function::eval(Xn1, Fn1),
		Function::eval(An, FAn),
		(	Fn1*FAn < 0.0 ->
			An1 is An,
			Bn1 is Xn1
		;	An1 is Xn1,
			Bn1 is Bn
		),
		bisection(Function, An1, Bn1, Xn1, Error, Zero).

:- end_object.



:- object(newton,
	implements(find_rootp)).

	:- info([
		version is 1.2,
		author is 'Paul Crocker... No More Coffee',
		date is 2007/07/06,
		comment is 'Newton algorithm.'
	]).

	find_root(Function, Xa, Xb, Deviation, Zero) :-
		Ac is (Xb - Xa) / 2,
		newton(Function, Xa, Ac, Deviation, Zero).

	newton(_, Zero, Ac, Deviation, Zero) :-
		abs(Ac) < Deviation,
		!.
	newton(Function, X0, Ac, Deviation, Zero) :-
		Xn1 is X0 + Ac,
		Function::eval(Xn1, Fx),
		Function::evald(Xn1, DFx),
		Ac2 is -(Fx/DFx),
		newton(Function, Xn1, Ac2, Deviation, Zero).

:- end_object.


:- object(muller,
	implements(find_rootp)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura and Paulo Nunes',
		date is 2006/11/26,
		comment is 'Muller algorithm.'
	]).

	find_root(Function, Xa, Xb, Deviation, Zero) :-
		Xc is (Xa + Xb) / 2.0,
		muller(Function, Xa, Xc, Xb, Deviation, Zero).

	muller(Function, Xa, Xb, Xc, Deviation, Zero) :-
		Function::eval(Xa, Ya),
		Function::eval(Xb, Yb),
		Function::eval(Xc, Yc),
		H1 is Xb - Xa,
		DDba is (Yb - Ya) / H1,
		Ac is Deviation + 1.0,
		muller(Function, Xa, Xb, Xc, Deviation, Ya, Yb, Yc, Ac, H1, DDba, Zero).

	muller(_, _, _, Xc, Deviation, _, _, _, Ac, _, _, Xc) :-
		abs(Ac) < Deviation,
		!.
	muller(Function, Xa, Xb, Xc, Deviation, _, Yb, Yc, _, _, DDba, Zero) :-
		H2n is Xc - Xb,
		DDcbn is (Yc - Yb) / H2n,
		Cn is (DDcbn - DDba) / (Xc - Xa),
		Bn is DDcbn + H2n * Cn,
		Rn is Bn * Bn - 4.0 * Yc * Cn,
		(	Rn < 0.0 ->
			fail
		;	V is sqrt(Rn)
		),
		(	Bn > 0.0 ->
			Dn is Bn + V
		;	Dn is Bn - V
		),
		Acn is -(2 * Yc / Dn),
		Xan is Xb,
		Xbn is Xc,
		Xcn is Xc + Acn,
		Yan is Yb,
		Ybn is Yc,
		Function::eval(Xcn, Ycn),
		H1n is H2n,
		DDban is DDcbn,
		muller(Function, Xan, Xbn, Xcn, Deviation, Yan, Ybn, Ycn, Acn, H1n, DDban, Zero).

:- end_object.



:- object(function_root,
	implements(find_rootp)).

	:- info([
		version is 2.1,
		author is 'Paulo Moura and Paulo Nunes',
		date is 2008/02/08,
		comment is 'Multi-threading interface to root finding algorithms.'
	]).

	:- threaded.

	:- public(find_root/6).
	:- mode(find_root(+object_identifier, +float, +float, +float, -float, -object_identifier), one).
	:- info(find_root/6, [
		comment is 'Finds the root of a function in the interval [A, B] given a maximum aproximation error. Returns the method used.',
		argnames is ['Function', 'A', 'B', 'Error', 'Zero', 'Method']
	]).

	find_root(Function, A, B, Error, Zero, Algorithm) :-
		threaded((
				(catch(bisection::find_root(Function, A, B, Error, Zero), _, fail), Algorithm = bisection)
			;	(catch(newton::find_root(Function, A, B, Error, Zero), _, fail), Algorithm = newton)
			;	(catch(muller::find_root(Function, A, B, Error, Zero), _, fail), Algorithm = muller)
			)).

	find_root(Function, A, B, Error, Zero) :-
		find_root(Function, A, B, Error, Zero, _).

:- end_object.
