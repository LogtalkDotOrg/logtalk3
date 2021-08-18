%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:7:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the "lambdas_compiled" example.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2, assertion/2
	]).

	test(lambdas_compiled_01, true(R == 25)) :-
		call([X,Y]>>(Y is X*X), 5, R).

	test(lambdas_compiled_02, true(T == 625)) :-
		call({X,Y,R}/[Z]>>(call([X,Y]>>(Y is X*X), 5, R), Z is R*R), T).

	test(lambdas_compiled_03, true) :-
		meta::map([X]>>(X>3),[4,5,9]).

	test(lambdas_compiled_04, true(Zs == [a-1, b-2, c-3])) :-
		meta::map([A-B,B-A]>>true, [1-a,2-b,3-c], Zs).

	test(lambdas_compiled_05, true(Zs == [a-1, b-2, c-3])) :-
		meta::map([A-B]>>([B-A]>>true), [1-a,2-b,3-c], Zs).

	test(lambdas_compiled_06, true) :-
		Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y),Z]>>(Z is sqrt(X*X + Y*Y)), Points, Distances),
		Distances = [Distance1, Distance2, Distance3],
		assertion(distance1, Distance1 =~= 4.1231056256176606),
		assertion(distance2, Distance2 =~= 5.3851648071345037),
		assertion(distance3, Distance3 =~= 8.5440037453175304),
		assertion(points, Points == [(1,4),(2,5),(8,3)]).

	test(lambdas_compiled_07, true) :-
		Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y)]>>([Z]>>(Z is sqrt(X*X + Y*Y))), Points, Distances),
		Distances = [Distance1, Distance2, Distance3],
		assertion(distance1, Distance1 =~= 4.1231056256176606),
		assertion(distance2, Distance2 =~= 5.3851648071345037),
		assertion(distance3, Distance3 =~= 8.5440037453175304),
		assertion(points, Points == [(1,4),(2,5),(8,3)]).

	test(lambdas_compiled_08, true(Result == [5, 25, 61])) :-
		meta::map([[X,Y],Z]>>(Z is X*X + Y*Y), [[1,2],[3,4],[5,6]], Result).

	test(lambdas_compiled_09, true(Result == [5, 25, 61])) :-
		meta::map([[X,Y]]>>([Z]>>(Z is X*X + Y*Y)), [[1,2],[3,4],[5,6]], Result).

	test(lambdas_compiled_10, true(Ysss == [[[4,5,6], [7]], [[8]]])) :-
		Xsss = [[[1,2,3], [4]], [[5]]],
		meta::map(meta::map(meta::map([X,Y]>>(Y is X+3))), Xsss, Ysss).

	test(lambdas_compiled_11, true(Ys == [[1], [2]])) :-
		meta::map([X,[X]]>>true,[1,2],Ys).

	test(lambdas_compiled_12, true(Xs == [1, 2])) :-
		meta::map([X,[X]]>>true,Xs,[[1],[2]]).

	test(lambdas_compiled_13, true(R == [1000, 124])) :-
		meta::map([N]>>({L}/[M]>>(list::length(L, N), list::length([1|L], M))), [999,123], R).

	test(lambdas_compiled_14, true) :-
		[]>>true.

	test(lambdas_compiled_15, true) :-
		{}/true.

	test(lambdas_compiled_16, true) :-
		{}/[]>>true.

	test(lambdas_compiled_17, true) :-
		{_}/true.

	test(lambdas_compiled_18, ball(error(representation_error(lambda_parameters), logtalk(_,_)))) :-
		logtalk << ({X}/[X]>>true).

	test(lambdas_compiled_19, false) :-
		meta::map({X}/[X]>>char_code(X), [a,b,c], _).

	test(lambdas_compiled_20, ball(error(representation_error(lambda_parameters), logtalk(_,_)))) :-
		meta::map([X,_,_]>>char_code(X), [a,b,c], _).

	test(lambdas_compiled_21, true(SolutionsSorted == [[dinar], [dinar], [euro], [euro], [pound_sterling], [ringgit]])) :-
		findall(Currencies, countries::currencies_wrong(Currencies), Solutions),
		list::msort(Solutions, SolutionsSorted).

	test(lambdas_compiled_22, true(Currencies == [dinar, euro, pound_sterling, ringgit])) :-
		countries::currencies_no_lambda(Currencies).

	test(lambdas_compiled_23, true(Currencies == [dinar, euro, pound_sterling, ringgit])) :-
		countries::currencies_lambda(Currencies).

	test(lambdas_compiled_24, true(R == 45)) :-
		sigma::sum([X,Y]>>(Y is X), 0, 9, R).

	test(lambdas_compiled_25, true(R == 285)) :-
		sigma::sum([X,Y]>>(Y is X*X), 0, 9, R).

	test(lambdas_compiled_26, true(R == 330)) :-
		sigma::sum([X,Y]>>(sigma::sum({X,Y}/[W,Z]>>(Z is W), X, 9, Y)), 0, 9, R).

	test(lambdas_compiled_27, true(R == 14)) :-
		meta::fold_left([X,Y,Z]>>(Z is X+Y*Y), 0, [1,2,3], R).

	test(lambdas_compiled_28, true(F == 55)) :-
		meta::fold_left([N1-[F1,F2],_,N2-[F2,F3]]>>(F3 is F1+F2, N2 is N1+1), 0-[0,1], _, 10-[F, _]).

	test(lambdas_compiled_29, true(R == [1, 1, 2, 6, 24, 120, 720])) :-
		meta::scan_left([X,Y,Z]>>(Z is X*Y), 1, [1,2,3,4,5,6], R).

	% the following two tests were contributed by Boris Vassilev

	test(lambdas_compiled_30, true(R == [a(1)])) :-
		meta::include([a(_)]>>true, [a(1), b(2)], R).

	test(lambdas_compiled_31, true(R == [a(1)])) :-
		meta::include([a(_)]>>true, [b(2), a(1)], R).

	test(lambdas_compiled_32, true(R == [])) :-
		meta::include([a(_)]>>true, [b(2), b(1)], R).

	test(lambdas_compiled_33, true(L == [1-1,1-2,2-1,2-2])) :-
		findall(A-B, meta::map([X]>>(X=1;X=2), [A,B]), L).

	test(lambdas_compiled_34, true(XX-YY == x-y)) :-
		call([X]>>f(X), XX, YY).

	test(lambdas_compiled_35, true(XX-YY == x-y)) :-
		call([X,Y]>>f(X,Y), XX, YY).

	test(lambdas_compiled_36, true(XX-YY == x-y)) :-
		call([X]>>({X}/[Y]>>f(X,Y)), XX, YY).

	test(lambdas_compiled_37, true) :-
		call([X1,Y1,Z1]>>plus(X1,Y1,Z1), 1, 2, 3),
		call([X2,Y2]>>plus(X2,Y2), 1, 2, 3),
		call([X3]>>plus(X3), 1, 2, 3),
		call([]>>plus, 1, 2, 3),
		call(plus, 1, 2, 3).

	test(lambdas_compiled_38, true(L == [1,2])) :-
		findall(X, {X}/p(X,_), L).

	% auxiliary predicates

	f(x, y).

	p(1,a).
	p(2,b).

	:- if(\+ predicate_property(plus(_,_,_), built_in)).

		plus(X, Y, Z) :-
			(	integer(X), integer(Y) -> Z is X + Y
			;	integer(X), integer(Z) -> Y is Z - X
			;	integer(Y), integer(Z) -> X is Z - Y
			;	fail
			).

	:- endif.

:- end_object.
