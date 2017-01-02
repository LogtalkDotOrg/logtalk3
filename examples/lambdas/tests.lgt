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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.4,
		author is 'Parker Jones and Paulo Moura',
		date is 2015/12/07,
		comment is 'Unit tests for the "lambdas" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	:- discontiguous(succeeds/1).
	:- discontiguous(throws/2).

	succeeds(lambdas_01) :-
		call([X,Y]>>(Y is X*X), 5, R),
		R == 25.

	succeeds(lambdas_02) :-
		call({X,Y,R}/[Z]>>(call([X,Y]>>(Y is X*X), 5, R), Z is R*R), T),
		T == 625.

	succeeds(lambdas_03) :-
		meta::map([X]>>(X>3),[4,5,9]).

	succeeds(lambdas_04) :-
		meta::map([X,Y]>>(X=A-B,Y=B-A), [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_05) :-
		meta::map([X,B-A]>>(X=A-B), [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_06) :-
		meta::map([A-B,B-A]>>true, [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_07) :-
		meta::map([A-B]>>([B-A]>>true), [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_08) :-
		Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y),Z]>>(Z is sqrt(X*X + Y*Y)), Points, Distances),
		Distances = [Distance1, Distance2, Distance3],
		Distance1 =~= 4.1231056256176606,
		Distance2 =~= 5.3851648071345037,
		Distance3 =~= 8.5440037453175304,
		Points == [(1,4),(2,5),(8,3)].

	succeeds(lambdas_09) :-
		Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y)]>>([Z]>>(Z is sqrt(X*X + Y*Y))), Points, Distances),
		Distances = [Distance1, Distance2, Distance3],
		Distance1 =~= 4.1231056256176606,
		Distance2 =~= 5.3851648071345037,
		Distance3 =~= 8.5440037453175304,
		Points == [(1,4),(2,5),(8,3)].

	succeeds(lambdas_10) :-
		meta::map([[X,Y],Z]>>(Z is X*X + Y*Y), [[1,2],[3,4],[5,6]], Result),
		Result == [5, 25, 61].

	succeeds(lambdas_11) :-
		meta::map([[X,Y]]>>([Z]>>(Z is X*X + Y*Y)), [[1,2],[3,4],[5,6]], Result),
		Result == [5, 25, 61].

	succeeds(lambdas_12) :-
		Xsss = [[[1,2,3], [4]], [[5]]],
		meta::map(meta::map(meta::map([X,Y]>>(Y is X+3))), Xsss,  Ysss),
		Ysss == [[[4,5,6], [7]], [[8]]].

	succeeds(lambdas_13) :-
		meta::map([X,[X]]>>true,[1,2],Ys),
		Ys == [[1], [2]].

	succeeds(lambdas_14) :-
		meta::map([X,[X]]>>true,Xs,[[1],[2]]),
		Xs == [1, 2].

	succeeds(lambdas_15) :-
		meta::map([N,M]>>(list::length(L, N), list::length([_|L], M)), [999,123],R),
		R == [1000, 124].

	succeeds(lambdas_16) :-
		meta::map([N]>>([M]>>(list::length(L, N), list::length([_|L], M))), [999,123],R),
		R == [1000, 124].

	succeeds(lambdas_17) :-
		[]>>true.

	succeeds(lambdas_18) :-
		{}/true.

	succeeds(lambdas_19) :-
		{}/[]>>true.

	succeeds(lambdas_20) :-
		{_}/true.

	throws(lambdas_21, error(representation_error(lambda_parameters), logtalk(_,_))) :-
		logtalk << ({X}/[X]>>true).

	fails(lambdas_22) :-
		meta::map({X}/[X]>>char_code(X), [a,b,c], _).

	throws(lambdas_23, error(representation_error(lambda_parameters), logtalk(_,_))) :-
		meta::map([X,_,_]>>char_code(X), [a,b,c], _).

	succeeds(lambdas_24) :-
		findall(Currencies, countries::currencies_wrong(Currencies), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [[dinar], [dinar], [euro], [euro], [pound_sterling], [ringgit]].

	succeeds(lambdas_25) :-
		countries::currencies_no_lambda(Currencies),
		Currencies == [dinar, euro, pound_sterling, ringgit].

	succeeds(lambdas_26) :-
		countries::currencies_lambda(Currencies),
		Currencies == [dinar, euro, pound_sterling, ringgit].

	succeeds(lambdas_27) :-
		sigma::sum([X,Y]>>(Y is X), 0, 9, R),
		R == 45.

	succeeds(lambdas_28) :-
		sigma::sum([X,Y]>>(Y is X*X), 0, 9, R),
		R == 285.

	succeeds(lambdas_29) :-
		sigma::sum([X,Y]>>(sigma::sum([W,Z]>>(Z is W), X, 9, Y)), 0, 9, R),
		R == 330.

	succeeds(lambdas_30) :-
		meta::fold_left([X,Y,Z]>>(Z is X+Y*Y), 0, [1,2,3], R),
		R == 14.

	succeeds(lambdas_31) :-
		meta::fold_left([N1-[F1,F2],_,N2-[F2,F3]]>>(F3 is F1+F2, N2 is N1+1), 0-[0,1], _, 10-[F, _]),
		F == 55.

	succeeds(lambdas_32) :-
		meta::scan_left([X,Y,Z]>>(Z is X*Y), 1, [1,2,3,4,5,6], R),
		R == [1, 1, 2, 6, 24, 120, 720].

	% the following two tests were contributed by Boris Vassilev

	succeeds(lambdas_33) :-
		meta::include([X]>>(X=a(_)), [a(1), b(2)], R),
		R == [a(1)].

	succeeds(lambdas_34) :-
		meta::include([a(_)]>>true, [a(1), b(2)], R),
		R == [a(1)].

	succeeds(lambdas_35) :-
		meta::include([a(_)]>>true, [b(2), a(1)], R),
		R == [a(1)].

	succeeds(lambdas_36) :-
		meta::include([a(_)]>>true, [b(2), b(1)], R),
		R == [].

	succeeds(lambdas_37) :-
		findall(A-B, meta::map([X]>>(X=1;X=2), [A,B]), L),
		L == [1-1,1-2,2-1,2-2].

	succeeds(lambdas_38) :-
		call(f, X, Y),
		call([X]>>f(X), X, Y),
		call([X,Y]>>f(X,Y), X, Y),
		call([X]>>({X}/[Y]>>f(X,Y)), X, Y),
		call(call(f, X), Y),
		call(f(X), Y),
		f(X, Y).

	succeeds(lambdas_39) :-
		call([X,Y,Z]>>plus(X,Y,Z), 1, 2, 3),
		call([X,Y]>>plus(X,Y), 1, 2, 3),
		call([X]>>plus(X), 1, 2, 3),
		call([]>>plus, 1, 2, 3),
		call(plus, 1, 2, 3).

	succeeds(lambdas_40) :-
		findall(X, {X}/p(X,_), L),
		L == [1,2].

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
